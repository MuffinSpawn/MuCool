module Main where

-- import Control.Monad.Loops
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception (try, catch, IOException)
import Control.Monad (forM)
import Control.Monad.STM
import Data.Int (Int32)
import Data.Word (Word, Word32, Word64)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Foreign.C.String (CString, newCString)
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr, nullPtr)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.Exit
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hFlush, BufferMode(..),
                  hIsWritable, hPutStr, hIsOpen, Handle, stderr, hGetChar,
                  hSetNewlineMode, noNewlineTranslation, NewlineMode(..),
                  Newline(..), openBinaryFile, IOMode(WriteMode), hClose,
                  openFile)
import System.IO.Error (isEOFError)

import MuCool.DAQmx
import MuCool.Localize (accumulatedCorrelation, listToChannelData,
                        kSamplingFrequency, loadMicrophoneCoordinates,
                        CartesianCoordinates2D, PolarCoordinates, ChannelData,
                        GridResolution2D(..), SampleWindow(..),
                        polarToCartesian, saveChannelData)
import MuCool.Time

------------------------------ Data Types --------------------------------------
data CylindricalCoordinates
  = CylindricalCoordinates { r :: Double, theta :: Double, z :: Double }
data BreakdownStatus = BreakdownStatus {
  eventIndex            :: Word64,
  breakdownIndex        :: Word64,
  upstreamCoordinates   :: CartesianCoordinates2D,
  downstreamCoordinates :: CartesianCoordinates2D }

-------------------- Hardware and Protocol Constants ---------------------------
kNumChannels = (fromIntegral 24) :: Word
kSamplesPerChannel = (fromIntegral 5120) :: Word
kCommandLength = (fromIntegral 4) :: Word

------------------------ DAQ Mode Constants ------------------------------------
kDAQModeStop          = 0
kDAQModeNormal        = 1
kDAQModeForceTrigger  = 2

----------------------- Function Definitions -----------------------------------

{- Setup the DAQmx Analog Input virtual channel.
 -}
setupAnalogInput = do
  -- setup analog input task
  aiTaskPtr <- malloc :: IO (Ptr Word64)
  c_DAQmxCreateTask nullPtr aiTaskPtr
  aiTask <- peek aiTaskPtr

  -- setup analog input voltage task
  {-
  physicalChannels  <-  newCString
      ("cDAQ9188-18A4729Mod1/ai0:cDAQ9188-18A4729Mod1/ai5,"
    ++ "cDAQ9188-18A4729Mod2/ai7:cDAQ9188-18A4729Mod2/ai2,"
    ++ "cDAQ9188-18A4729Mod3/ai0:cDAQ9188-18A4729Mod3/ai5,"
    ++ "cDAQ9188-18A4729Mod4/ai7:cDAQ9188-18A4729Mod4/ai2")
  -}
  physicalChannels  <- newCString ("cDAQ1Mod1/ai0:cDAQ1Mod1/ai5,"
                               ++  "cDAQ1Mod2/ai7:cDAQ1Mod2/ai2,"
                               ++  "cDAQ1Mod3/ai0:cDAQ1Mod3/ai5,"
                               ++  "cDAQ1Mod4/ai7:cDAQ1Mod4/ai2")
  aiChannelName     <- newCString "csm"
  let terminalConfig    = c_DAQmx_Val_Cfg_Default 
      minVal            = (realToFrac (-5.0)) :: Double
      maxVal            = (realToFrac    5.0) :: Double
      aiUnits           =  c_DAQmx_Val_Volts
      customScaleName   = nullPtr :: CString
  c_DAQmxCreateAIVoltageChan aiTask physicalChannels aiChannelName
                             terminalConfig minVal maxVal aiUnits
                             customScaleName

  -- setup sample clock
  -- clkSource <- newCString "cDAQ9188-18A4729/Ctr0InternalOutput"
  -- clkSource <- newCString "cDAQ1/Ctr0InternalOutput"
  -- clkSource <- newCString "cDAQ1/ai/SampleClock"
  let clkSource         = nullPtr :: CString
  let terminalConfig    = c_DAQmx_Val_Cfg_Default 
      sampleFreq        = (realToFrac 100000.0) :: Double
      activeEdge        = c_DAQmx_Val_Rising
      aiSampleMode      = c_DAQmx_Val_ContSamps 
      samplesPerChannel = (fromIntegral kSamplesPerChannel) :: Word64
  c_DAQmxCfgSampClkTiming aiTask clkSource sampleFreq activeEdge aiSampleMode
                          samplesPerChannel
  c_DAQmxCfgInputBuffer aiTask
                        ((fromIntegral (samplesPerChannel * 10)) :: Word32)
  return aiTask

{- Setup the DAQmx counter that generates sample clock pulses. This is necessary
 - since the DAQ cards are not retriggerable, but all counters are.
 -}
setupSampleCounter = do
  -- setup counter task
  counterTaskPtr <- malloc :: IO (Ptr Word64)
  c_DAQmxCreateTask nullPtr counterTaskPtr
  counterTask <- peek counterTaskPtr

  -- setup retriggerable counter
  counter <- newCString "cDAQ9188-18A4729/_ctr0"
  let counterChannelName  = nullPtr :: CString
      counterUnits        = c_DAQmx_Val_Hz
      idleState           = c_DAQmx_Val_Low
      initialDelay        = (realToFrac 0.0) :: Double
      sampleFreq          = (realToFrac 100000.0) :: Double
      dutyCycle           = (realToFrac 0.5) :: Double
  c_DAQmxCreateCOPulseChanFreq counterTask counter counterChannelName
                               counterUnits idleState initialDelay sampleFreq
                               dutyCycle
  let counterSampleMode = c_DAQmx_Val_FiniteSamps
      samplesPerChannel = (fromIntegral kSamplesPerChannel) :: Word64
  c_DAQmxCfgImplicitTiming counterTask counterSampleMode samplesPerChannel
  -- triggerSource <- newCString "cDAQ9188-18A4729/PFI0"
  triggerSource <- newCString "cDAQ1/PFI0"
  let triggerEdge = c_DAQmx_Val_Rising
  c_DAQmxCfgDigEdgeStartTrig counterTask triggerSource triggerEdge
  let retriggerable = (fromIntegral 1) :: Word32
  c_DAQmxSetStartTrigRetriggerable counterTask retriggerable
  return counterTask

{- Fetch a chunk of data from the DAQ corresponding to the given number of
 - samples for each channel. The data is reformatted into a 2D list that is
 - indexed first by channel.
 -}
fetch :: Word64 -> IO ChannelData
fetch aiTask = do
  let numSamplesPerChannel  = (fromIntegral kSamplesPerChannel) :: Int32
      timeout     = (realToFrac (-1.0)) :: Double
      fillMode    = c_DAQmx_Val_GroupByChannel
      numChannels = (fromIntegral kNumChannels) :: Word32
      arraySize
        = numChannels * ((fromIntegral numSamplesPerChannel) :: Word32)
      reserved    = nullPtr :: (Ptr Word32)
  readArrayPtr
    <- (mallocArray ((fromIntegral arraySize) :: Int)) :: IO (Ptr Double)
  samplesPerChannelReadPtr <- malloc :: IO (Ptr Int32)
  rc_Read <- c_DAQmxReadAnalogF64 aiTask
                                  numSamplesPerChannel
                                  timeout
                                  fillMode
                                  readArrayPtr
                                  arraySize
                                  samplesPerChannelReadPtr
                                  reserved
  if (rc_Read /= 0)
  then do
    msg_Read <- getErrorMessage rc_Read
    putStrLn $ "Read Error Message: " ++ (show msg_Read)
  else return ()
  samplesPerChannelRead <- peek samplesPerChannelReadPtr
  -- FIXME: check that samplesPerChannelRead == numSamplesPerChannel
  flatChannelData <- peekArray (fromIntegral arraySize) readArrayPtr
  print $ show (length flatChannelData)
  return (listToChannelData flatChannelData kNumChannels)

{- Accept only one client connection and then process commands from that client.
 -}
serveData :: Socket -> TVar Int -> IO ()
serveData sock daqMode = do
  (handle, clientHost, clientPort) <- accept sock
  putStrLn $ "Client Connection: " ++
             (show clientHost) ++ ":" ++ (show clientPort)
  hSetBuffering handle NoBuffering
  hSetNewlineMode handle noNewlineTranslation
  processCommands handle daqMode

{- Get the requisite number of characters from the client that
 - represents a command name string. Repeated calls to hGetChar
 - are required since hGetLine waits for EOF which only happens
 - after the client closes it's connection.
 -}
getCommand :: Handle -> IO String
getCommand handle = do
  let ops = replicate ((fromIntegral kCommandLength) :: Int) (hGetChar handle)
  forM ops (\op -> op)

{- Process commands received from the client. The available commands are
 -  "die!"  - Shutdown everything.
 -  "stat"  - Get the status. At the moment this is just the number of
 -            results waiting to be served to the client.
 -  "rtrv"  - Retrieve the next result. The response is a variable-length
 -            message that includes the event ID (number of primary triggers
 -            minus 1), the spark ID (number of sparks and forced-triggers
 -            minus 1) if the secondary trigger was raised, and the spark
 -            coordinates if the secondary trigger was raised.
 -  "frce"  - Force a secondary trigger.
 -}
processCommands :: Handle -> TVar Int -> IO ()
processCommands handle daqMode = do
  isHandleOpen <- hIsOpen handle
  putStrLn $ "Handle is open? " ++ (show isHandleOpen)
  -- input <- try (hGetLine handle)
  input <- try (getCommand handle)
  case input of
    Left e ->
      if isEOFError e then do
        atomically $ writeTVar daqMode kDAQModeStop
        putStrLn $ "EOF detected from client socket. Stopping DAQ..."
        return ()
      else ioError e
    Right line -> do
      let cmd = words line
      case (head cmd) of
        ("die!") -> do
          atomically $ writeTVar daqMode kDAQModeStop
          putStrLn $ "Received kill command from client. Stopping DAQ..."
        {-
        ("status") -> catch (hPutStr handle "Not Implemented\CR\LF") (\e -> do 
          let error = show (e :: IOException)
          putStrLn(  "Warning: failed to respond to client status "
                         ++ "request: " ++ error)
          return ())
        -}
        ("stat") -> do
          hPutStr handle "15Not Implemented"
        ("rtrv") -> do
          hPutStr handle "15Not Implemented"
        ("frce") -> do
          atomically $ writeTVar daqMode kDAQModeForceTrigger
          putStrLn $ "Received force-trigger command from client. "
                  ++ "uForcing secondary trigger..."
        _ -> do
          putStrLn $ "Unknown command \"" ++ (head cmd) ++ "\""
      hFlush handle
      mode <- atomically $ readTVar daqMode
      if (mode < 1) then return ()
      else processCommands handle daqMode

{- Start the DAQ tasks and the data processing loop.
 - Clean up when the loop is finished.
 -}
monitorDAQ :: TVar Int -> TChan BreakdownStatus -> String -> IO ()
monitorDAQ daqMode queue dataFilename = do
  aiTask <- setupAnalogInput
  rc_StartAITask <- c_DAQmxStartTask aiTask
  putStrLn ("Start AI Task RC: " ++ (show rc_StartAITask))
  if (rc_StartAITask /= 0)
  then do
    msg_StartAITask <- getErrorMessage rc_StartAITask
    putStrLn $ "Start AI Task Error Message: " ++ (show msg_StartAITask)
  else return ()

  {-
  counterTask <- setupSampleCounter
  rc_StartCounterTask <- c_DAQmxStartTask counterTask
  putStrLn $ "Start Counter Task RC: " ++ (show rc_StartCounterTask)
  if (rc_StartCounterTask /= 0)
  then do
    msg_StartCounterTask <- getErrorMessage rc_StartCounterTask
    putStrLn $ "Start Counter Task Error Message: "
             ++ (show msg_StartCounterTask)
  else return ()
  -}

  -- Process data until daqMode == kDAQModeStop
  let eventIndex = 0
      breakdownIndex = 0
      rfHammers = []
  micCoordinates <- loadMicrophoneCoordinates
    "C:\\Users\\plane\\Desktop\\MTA\\SCM with cu Windows.cfg"
  dataHandle <- openBinaryFile dataFilename WriteMode
  let filenameSize = length dataFilename
      reducedFilename = (take (filenameSize-4) dataFilename) ++ "_reduced.bin"
      logFilename = (take (filenameSize-4) dataFilename) ++ ".log"
  reducedHandle <- openBinaryFile reducedFilename WriteMode
  logHandle <- openFile logFilename WriteMode
  processData aiTask
              daqMode
              queue
              (map polarToCartesian micCoordinates)
              [dataHandle, reducedHandle, logHandle]
              rfHammers
              eventIndex
              breakdownIndex
  hClose dataHandle
  hClose reducedHandle
  hClose logHandle

  -- clear tasks
  -- rc_counterClearTask <- c_DAQmxClearTask counterTask
  rc_aiClearTask <- c_DAQmxClearTask aiTask
  return ()

{- Fetch data from the DAQ and process it to determine a) whether there was a
 - spark, and b) where the spark occured. Archive the data for offline analysis.
 -}
processData :: Word64 -> TVar Int -> TChan BreakdownStatus
            -> [CartesianCoordinates2D] -> [Handle] -> [ChannelData]
            -> Word64 -> Word64 -> IO ()
processData aiTask daqMode queue micCoordinates fileHandles
            rfHammers eventIndex breakdownIndex = do
  {-
  availableSamplesPtr <- malloc :: IO (Ptr Word32)  -- per channel
  c_DAQmxGetReadAvailSampPerChan aiTask availableSamplesPtr
  availableSamples <- peek availableSamplesPtr
  putStrLn ("Available Samples per Channel: " ++ (show availableSamples))
  -}
  
  let f_s = kSamplingFrequency
      resolution = GridResolution2D {nx = 16, ny = 16}
      windows = (SampleWindow {offset   = truncate (f_s / 6.5e-4),
                               duration = truncate (f_s / 2.0e-4)},
                 SampleWindow {offset   = truncate (f_s / 8.5e-4),
                               duration = truncate (f_s / 2.0e-4)})
  signals <- fetch aiTask
  breakdownIndex' <- processFetch daqMode
                                  queue
                                  eventIndex
                                  breakdownIndex
                                  signals
                                  micCoordinates
                                  fileHandles
                                  rfHammers
                                  resolution
                                  windows
  let eventIndex' = eventIndex + 1
      rfHammers' = pushRFHammer signals
                                (breakdownIndex' - breakdownIndex)
                                rfHammers

  mode <- atomically $ readTVar daqMode
  if (mode < 1) then do
    putStrLn $ "Stop DAQ signal received. Halting DAQ loop..."
    return ()
  else processData aiTask
                   daqMode
                   queue
                   micCoordinates
                   fileHandles
                   rfHammers'
                   eventIndex'
                   breakdownIndex'

processFetch :: TVar Int -> TChan BreakdownStatus -> Word64 -> Word64
             -> ChannelData -> [CartesianCoordinates2D] -> [Handle]
             -> [ChannelData] -> GridResolution2D
             -> (SampleWindow, SampleWindow) -> IO (Word64)
processFetch daqMode queue evtIndex bdIndex signals micCoordinates
             fileHandles rfHammers resolution windows = do
  mode <- atomically $ readTVar daqMode
  if (mode == kDAQModeStop) then
    return (bdIndex)
  else if ((mode == kDAQModeForceTrigger) || (isBreakdown signals)) then do
    let reducedSignals = removeRFHammer signals rfHammers
        -- Assuming data filename ends with ".bin"
    saveChannelData (fileHandles !! 0) signals
    saveChannelData (fileHandles !! 1) reducedSignals
    logEvent (fileHandles !! 2) bdIndex mode
    let numChannelsPerSide = quot kNumChannels 2
        upstreamCoords = accumulatedCorrelation
          (take (fromIntegral numChannelsPerSide) reducedSignals)
          (take (fromIntegral numChannelsPerSide) micCoordinates)
          resolution
          (fst windows)
        downstreamCoords = accumulatedCorrelation
          (drop (fromIntegral numChannelsPerSide) reducedSignals)
          (drop (fromIntegral numChannelsPerSide) micCoordinates)
          resolution
          (snd windows)
    atomically $ writeTChan queue BreakdownStatus {
      eventIndex = evtIndex,
      breakdownIndex = bdIndex,
      upstreamCoordinates = upstreamCoords,
      downstreamCoordinates = downstreamCoords}
    atomically $ setNormalDAQMode daqMode
    return (bdIndex + 1)  -- increment the breakdown index
  else
    return (bdIndex)

logEvent :: Handle -> Word64 -> Int -> IO ()
logEvent logHandle index mode = do
  timestamp <- getTimestamp
  let logRecord = (show index) ++ " " ++ timestamp
  if (mode == kDAQModeForceTrigger) then
    hPutStrLn logHandle (logRecord ++ " *Forced*")
  else
    hPutStrLn logHandle logRecord
  
setNormalDAQMode :: TVar Int -> STM ()
setNormalDAQMode daqMode = do
  mode <- readTVar daqMode
  if (mode == kDAQModeStop) then return ()
  else writeTVar daqMode kDAQModeNormal

{- Calculate the running average of the RF  hammer signals and subtract them
 - from the spark signals.
 -}
removeRFHammer :: ChannelData -> [ChannelData] -> ChannelData
removeRFHammer signals rfHammers
  | (length rfHammers) > 0 = zipWith (zipWith (-)) signals avgRFHammer
  | otherwise = signals
    where numRFHammers = realToFrac (length rfHammers)
          rfHammerSum = foldl1 (zipWith (zipWith (+))) rfHammers
          avgRFHammer = map (map (/ 3.0)) rfHammerSum

isBreakdown :: ChannelData -> Bool
isBreakdown [] = False
isBreakdown signals = foldl1 (||) (map checkSignal signals)
  where checkSignal :: [Double] -> Bool
        checkSignal signal
          | ((length triggerRegion) == 0)
            = error ("Trigger region is empty! " ++ show (length signal))
          | otherwise = foldl1 (||) (map (>= voltageThreshold) triggerRegion)
          where f_s = kSamplingFrequency
                gateOffset = truncate (20.0e-3 * f_s)   -- 2000 Samples
                gateDuration = truncate (0.1e-3 * f_s)  -- 10 Samples
                triggerRegion = take gateDuration (drop gateOffset signal)
                voltageThreshold = 2.5

pushRFHammer :: ChannelData -> Word64 -> [ChannelData] -> [ChannelData]
pushRFHammer signals breakdownCount rfHammers
  | (breakdownCount == 0) && ((length rfHammers) < 3) = signals:rfHammers
  | (breakdownCount == 0) = signals:(take 2 rfHammers)
  | otherwise = rfHammers

waitForDAQ :: TVar Bool -> IO ()
waitForDAQ finished = do
  end <- atomically $ readTVar finished
  if end then return ()
  else do
    threadDelay $ truncate 1e4  -- 10 ms
    waitForDAQ finished

main = withSocketsDo $ do
  args <- getArgs
  if (length args == 0) then System.Exit.exitWith (ExitFailure 1)
  else return ()
  let port = fromIntegral (read $ head args :: Int)
      dataFilename = head (drop 1 args)

  -- DAQ thread modes: 0 = stop, 1 = normal, 2 = force secondary trigger
  daqMode <- atomically $ newTVar kDAQModeNormal

  queue <- atomically $ (newTChan :: STM (TChan BreakdownStatus))

  finished <- atomically $ newTVar False
  forkFinally (monitorDAQ daqMode queue dataFilename)
              (\_ -> atomically $ writeTVar finished True)

  sock <- listenOn $ PortNumber port
  putStrLn $ "Listening on " ++ (head args)
  putStrLn $ "Writing data to " ++ dataFilename
  serveData sock daqMode

  waitForDAQ finished
