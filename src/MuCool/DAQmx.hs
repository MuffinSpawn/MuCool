{-# LANGUAGE ForeignFunctionInterface #-}

module MuCool.DAQmx where

import Data.Int
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr

-- Terminal configuration constants from NIDAQmx.h
c_DAQmx_Val_Cfg_Default = (fromIntegral  (-1)) :: Int32  -- RSE
c_DAQmx_Val_RSE =         (fromIntegral 10083) :: Int32  -- RSE
c_DAQmx_Val_NRSE =        (fromIntegral 10078) :: Int32  -- NRSE
c_DAQmx_Val_Diff =        (fromIntegral 10106) :: Int32  -- Differential
c_DAQmx_Val_PseudoDiff =  (fromIntegral 12529) :: Int32  -- Pseudodifferential

-- Unit constants from NIDAQmx.h
c_DAQmx_Val_Volts =       (fromIntegral 10348) :: Int32  -- Volts
c_DAQmx_Val_Hz =          (fromIntegral 10373) :: Int32  -- Hz

-- Idle state constants from NIDAQmx.h
c_DAQmx_Val_Low =         (fromIntegral 10214) :: Int32

-- Active edge constants from NIDAQmx.h
c_DAQmx_Val_Rising =      (fromIntegral 10280) :: Int32

-- Sample mode constants from NIDAQmx.h
c_DAQmx_Val_FiniteSamps = (fromIntegral 10178) :: Int32
c_DAQmx_Val_ContSamps =   (fromIntegral 10123) :: Int32

-- Fill mode constants from NIDAQmx.h
c_DAQmx_Val_GroupByChannel    = (fromIntegral 1) :: Word32
c_DAQmx_Val_GroupByScanNumber = (fromIntegral 0) :: Word32

-------------- DAQmx function wrappers --------------------------
foreign import ccall unsafe "DAQmxGetErrorString"
  c_DAQmxGetErrorString :: Int32 -> CString -> Word32 -> IO Int32

foreign import ccall unsafe "DAQmxCreateTask"
  c_DAQmxCreateTask :: CString -> Ptr Word64 -> IO Int32

foreign import ccall unsafe "DAQmxStartTask"
  c_DAQmxStartTask :: Word64 -> IO Int32

foreign import ccall unsafe "DAQmxStopTask"
  c_DAQmxStopTask :: Word64 -> IO Int32

foreign import ccall unsafe "DAQmxClearTask"
  c_DAQmxClearTask :: Word64 -> IO Int32

foreign import ccall unsafe "DAQmxGetTaskName"
  c_DAQmxGetTaskName :: Word64 -> CString -> Word32 -> IO Int32

foreign import ccall unsafe "DAQmxCreateAIVoltageChan"
  c_DAQmxCreateAIVoltageChan :: Word64 -> CString -> CString -> Int32 -> Double
                             -> Double -> Int32 -> CString -> IO Int32

foreign import ccall unsafe "DAQmxCfgSampClkTiming"
  c_DAQmxCfgSampClkTiming :: Word64 -> CString -> Double -> Int32 -> Int32
                             -> Word64 -> IO Int32

foreign import ccall unsafe "DAQmxGetSampClkRate"
  c_DAQmxGetSampClkRate :: Word64 -> Ptr Double -> IO Int32

foreign import ccall unsafe "DAQmxCreateCOPulseChanFreq"
  c_DAQmxCreateCOPulseChanFreq :: Word64 -> CString -> CString -> Int32 -> Int32
                             -> Double -> Double -> Double -> IO Int32

foreign import ccall unsafe "DAQmxCfgImplicitTiming"
  c_DAQmxCfgImplicitTiming :: Word64 -> Int32 -> Word64 -> IO Int32

foreign import ccall unsafe "DAQmxCfgDigEdgeStartTrig"
  c_DAQmxCfgDigEdgeStartTrig :: Word64 -> CString -> Int32 -> IO Int32

foreign import ccall unsafe "DAQmxSetStartTrigRetriggerable"
  c_DAQmxSetStartTrigRetriggerable :: Word64 -> Word32 -> IO Int32

foreign import ccall unsafe "DAQmxCfgInputBuffer"
  c_DAQmxCfgInputBuffer :: Word64 -> Word32 -> IO Int32

foreign import ccall unsafe "DAQmxReadAnalogF64"
  c_DAQmxReadAnalogF64 :: Word64 -> Int32 -> Double -> Word32 -> Ptr Double
                        -> Word32 -> Ptr Int32 -> Ptr Word32 -> IO Int32

foreign import ccall unsafe "DAQmxGetReadAvailSampPerChan"
  c_DAQmxGetReadAvailSampPerChan :: Word64 -> Ptr Word32 -> IO Int32

foreign import ccall unsafe "DAQmxIsTaskDone"
  c_DAQmxIsTaskDone :: Word64 -> Ptr Word32 -> IO Int32


-------------- DAQmx helper functions -----------------------
getErrorMessage :: Int32 -> IO String
getErrorMessage errorCode = do
  error_char_pointer <- mallocArray 512
  c_DAQmxGetErrorString errorCode error_char_pointer 512
  peekCString error_char_pointer

{-
testDAQmxGetErrorString = do
  error_char_pointer <- mallocArray 512
  c_DAQmxGetErrorString (-313206) error_char_pointer 512
  error_string <- (peekCString error_char_pointer)
  putStrLn (show error_string)

testDAQmxCreateTask = do
  aiTaskPtr <- malloc :: IO (Ptr Word64)
  c_DAQmxCreateTask nullPtr aiTaskPtr
  aiTask <- peek aiTaskPtr
  putStrLn (show aiTask)
  c_DAQmxClearTask aiTask
-}
