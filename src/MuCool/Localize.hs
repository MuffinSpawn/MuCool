module MuCool.Localize where

import Control.Monad (forM)
import Debug.Trace (trace)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (Get, runGet, getWord32le, getWord64le)
import Data.Binary.Put (Put, runPut, putWord32le, putWord64le)
import Data.Int (Int32)
import Data.ReinterpretCast (doubleToWord, wordToDouble)
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Ptr
-- import Foreign.Marshall.Array
import System.IO (Handle, hFlush)

import MuCool.Signals

------------------------------ Data Types --------------------------------------
type ChannelData = [[Double]]
data CartesianCoordinates2D = CartesianCoordinates2D {  xCoord2D :: Double,
                                                        yCoord2D :: Double }
     deriving (Show)
data PolarCoordinates = PolarCoordinates {  r     :: Double,
                                            theta :: Double }
     deriving (Show)
data GridResolution2D = GridResolution2D { nx :: Int, ny :: Int }
     deriving (Show)
data SampleWindow = SampleWindow { offset :: Int, duration :: Int }
     deriving (Show)

-------------------- Material and Hardware Constants ---------------------------
kCavityRadius         = 60.0      -- cm
-- kSpeedOfSoundInCopper = 3.901e5   -- cm/s
kSpeedOfSoundInCopper = 4.8e5   -- cm/s
-- kSpeedOfSoundInCopper = 3.6e5   -- cm/s
kSamplingFrequency    = 1.0e5     -- S/s

----------------------- Function Definitions -----------------------------------
{- NOTE: Functions intended for public use follow the capitalize-first-letter
 - naming style. Helper functions use the underscore-word-separator style.
 -}
 

polarToCartesian :: PolarCoordinates -> CartesianCoordinates2D
polarToCartesian polarCoords = 
  CartesianCoordinates2D { xCoord2D = r' * cos theta', yCoord2D = r' * sin theta' }
  where r' = r polarCoords
        theta' = theta polarCoords

{- Load the polar coordinates of the microphones from the file with
 - the given name.
 -}
loadMicrophoneCoordinates :: String -> IO ([PolarCoordinates])
loadMicrophoneCoordinates filename = do
  config <- BL.readFile filename
  return (runGet deserializeMicrophoneCoordinates config)
  where deserializeMicrophoneCoordinates :: Get ([PolarCoordinates])
        deserializeMicrophoneCoordinates = do
          sizeWord <- getWord32le
          let size = (fromIntegral sizeWord) :: Int32
          forM [0..11] (\x -> do
            x_i <- getWord64le
            y_i <- getWord64le
            return (PolarCoordinates { r      = wordToDouble x_i,
                                       theta  = wordToDouble y_i }))

loadChannelData :: Handle -> IO (ChannelData)
loadChannelData handle = do
  config <- BL.hGetContents handle
  return (runGet deserializeChannelData config)
  where deserializeChannelData :: Get (ChannelData)
        deserializeChannelData = do
          rowsWord <- getWord32le
          columnsWord <- getWord32le
          let numRows = (fromIntegral rowsWord) :: Int32
              numColumns = (fromIntegral columnsWord) :: Int32
          forM [0..(numRows-1)] (\row -> do
            forM [0..(numColumns-1)] (\column -> do
              sample <- getWord64le
              return (wordToDouble sample)))

saveChannelData :: Handle -> ChannelData -> IO ()
saveChannelData handle signals = do
  BL.hPut handle (runPut serializeChannelData)
  hFlush handle
  where numChannels = length signals
        signalLength = length (head signals)
        serializeChannelData :: Put
        serializeChannelData = do
          putWord32le $ fromIntegral numChannels
          putWord32le $ fromIntegral signalLength
          forM signals (\signal -> do
            forM signal (\sample -> do
              putWord64le (doubleToWord sample)))
          return ()

{- Take a flat, 1D list of samples grouped by channel and convert it into a
 - 2D list of signals with the first dimension corresponding to the channel.
 -}
listToChannelData :: [Double] -> Word -> ChannelData
listToChannelData [] _ = []
listToChannelData list num_channels
  = (take num_samples_per_channel list)
  : (listToChannelData (drop num_samples_per_channel list) (num_channels - 1))
  where num_samples_per_channel
          = quot (length list) ((fromIntegral num_channels) :: Int)

{- Localize a sound source for a set of microphone signals using the
 - Accumulated Correlation (AC) algorithm. AC uses the principle of
 - least commitment by holding onto the entirety of all the cross-correlation
 - vectors formed between all of the signals. The elements of those vectors are
 - then summed according to the expected time delay between candidate
 - sound sources and the microphones. The most likely candidate sound source is
 - taken as the one with the largest sum.
 -}
accumulatedCorrelation :: ChannelData -> [CartesianCoordinates2D]
                       -> GridResolution2D -> SampleWindow
                       -> CartesianCoordinates2D
accumulatedCorrelation signals mic_locations resolution window
  | (length signals) /= (length mic_locations)
    = error "Number of signals and microphones are not the same." 
  | (nx resolution) <= 0
    = error "The number of candidate x coordintes (nx) must be >= 1"
  | ((ny resolution) <= 0)
    = error "The number of candidate y coordintes (ny) must be >= 1"
  | ((offset window) < 0)
    = error "The time sample offset (t_offset) must be >= 0"
  | ((duration window) < 0)
    = error "The time sample duration (t_duration) must be >= 0"
  | otherwise = CartesianCoordinates2D { xCoord2D = x_coords !! i,
                                         yCoord2D = y_coords !! j }
  where radius = kCavityRadius
        corr_sums = cross_correlation_sums signals
                                           mic_locations
                                           resolution
                                           window
        indicies = fst (max_index_2D corr_sums)
        i = fst indicies
        j = snd indicies
        x_coords = coordinate_set radius (nx resolution)
        y_coords = coordinate_set radius (ny resolution)

{- Generate a matrix with elements corresponding to the sums of
 - cross-correlation values for the delays between each microphone and the
 - corresponding candidate location. The elements are indexed by the candidate
 - location's x index (row) and y index (column).
 -}
cross_correlation_sums
  :: ChannelData -> [CartesianCoordinates2D] -> GridResolution2D -> SampleWindow
  -> [[Double]]
cross_correlation_sums signals mic_locations resolution window
  = [
      [sum
        [sum
          [element  corr_matrix
                    mic_locations
                    CartesianCoordinates2D { xCoord2D = (x_coords !! x_index),
                                             yCoord2D = (y_coords !! y_index) }
                    i
                    j
              | j <- [(i+1),(i+2)..(mic_count-1)]
          ] | i <- [0, 1..(mic_count-1)]
        ] | y_index <- [0, 1..((ny resolution)-1)]
      ] | x_index <- [0, 1..((nx resolution)-1)]
    ]
  where correlation_regions
          = [take (duration window) (drop (offset window) signal)
             | signal <- signals]
        corr_matrix = cross_correlation_matrix correlation_regions
        radius = kCavityRadius
        x_coords = coordinate_set radius (nx resolution)
        y_coords = coordinate_set radius (ny resolution)
        mic_count = length mic_locations
        element :: [[[Double]]] -> [CartesianCoordinates2D]
                -> CartesianCoordinates2D -> Int -> Int -> Double
        element corr_matrix mic_coords source_coords i j
          = corr_matrix !! i !! j !! (i0 + round ((tau_j_q - tau_i_q) * f_s) - 1)
          where v_cu = kSpeedOfSoundInCopper
                f_s = kSamplingFrequency
                i0 = length (head correlation_regions)
                tau_i_q = coords_distance source_coords (mic_coords !! i)
                        / v_cu
                tau_j_q = coords_distance source_coords (mic_coords !! j)
                        / v_cu

-- dx = 2*radius/nx, x = -(nx-1)/2 * dx + n * dx = (1/2 - nx/2 + n)*dx = (1-ny+2*n)*radius/ny
coordinate_set :: Double -> Int -> [Double]
coordinate_set radius numCoords
  = [(1.0 - (realToFrac numCoords)+(2.0*(realToFrac n)))
     * radius/(realToFrac numCoords) | n <- [0..(numCoords-1)]]


{- Find the indicies and value of the maximum in a 2D list/matrix of numbers.
 -}
max_index_2D :: (Num a1, Ord a) => [[a]] -> ((a1, a1), a)
max_index_2D list = recurse list (0, 0) where
  recurse :: (Num a1, Ord a) => [[a]] -> (a1, a1) -> ((a1, a1), a)
  recurse [] _ = error "No maximum defined for an empty set."
  recurse [x] (i, j) = ((i, x_max_index), x_max_val) 
    where x_max = max_index x
          x_max_index = fst x_max
          x_max_val = snd x_max
  recurse (x:xs) (i, j)
    | x_max_val > xs_max_val = ((i, x_max_index), x_max_val)
    | x_max_val <= xs_max_val = xs_max
    where x_max = max_index x
          x_max_index = fst x_max
          x_max_val = snd x_max
          xs_max = recurse xs (i+1, j)
          xs_max_index = fst xs_max
          xs_max_val = snd xs_max

{- Find the index and value of the maximum in a list of numbers.
 -}
max_index :: (Num a1, Ord a) => [a] -> (a1, a)
max_index list = recurse list 0 where
  recurse :: (Num a1, Ord a) => [a] -> a1 -> (a1, a)
  recurse [] _ = error "No maximum defined for an empty set."
  recurse [x] index = (index, x) 
  recurse (x:xs) index
    | x > (snd xs_max_index) = (index, x)
    | x <= (snd xs_max_index) = xs_max_index
    where xs_max_index = recurse xs (index+1)

{- Find the distance between a set of 2D Cartesian coordinates
 -}
coords_distance :: CartesianCoordinates2D -> CartesianCoordinates2D -> Double
coords_distance a b
  = sqrt ((xCoord2D a - xCoord2D b)**2 + (yCoord2D a - yCoord2D b)**2)

coords_distance' :: (Double,Double) -> (Double,Double) -> Double
coords_distance' (x1,y1) (x2,y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

delayMatrix :: ChannelData -> SampleWindow -> [[Double]]
delayMatrix signals window = map (map (delay . fst . max_index)) corr_matrix
  where correlation_regions
          = [take (duration window) (drop (offset window) signal)
             | signal <- signals]
        corr_matrix = cross_correlation_matrix correlation_regions
        delay :: Int -> Double
        delay index = (realToFrac (index - i0)) / (realToFrac kSamplingFrequency)
          where i0 = (length (head correlation_regions)) - 1

{- Create a 3D cross-correlation matrix where the first and second dimensions
 - correspond to the channel/signal pairs, and the third dimension corresponds
 - the time delay
 -}
cross_correlation_matrix :: ChannelData -> [[[Double]]]
cross_correlation_matrix signals
  = [recurse signal signals | signal <- signals] where
    recurse :: [Double] -> [[Double]] -> [[Double]]
    recurse [] _ = error "Empty signal."
    recurse signal [] = []
    recurse signal [x] = (crossCorrelation signal x):(recurse signal [])
    recurse signal (x:xs) = (crossCorrelation signal x):(recurse signal xs)

listToCoordinates :: [[Double]] -> [CartesianCoordinates2D]
listToCoordinates [] = []
-- listToCoordinates [elem] = [CartesianCoordinates2D { xCoord2D = (elem !! 0), yCoord2D = (elem !! 1) }]
listToCoordinates (elem:elems) =
  CartesianCoordinates2D { xCoord2D=(elem !! 0), yCoord2D=(elem !! 1) }
  :(listToCoordinates elems)

-------------------------Foreign Wrapper Functions------------------------------

accumulated_correlation :: Ptr CDouble -> CUInt -> Ptr CDouble -> CUInt -> CUInt
                        -> CUInt -> CUInt -> CUInt -> Ptr CDouble -> IO ()
accumulated_correlation  signals
                         signals_size
                         mic_locations
                         mic_locations_size
                         nx
                         ny
                         t_offset
                         t_duration
                         coordinates = do
  let num_signals = quot (fromIntegral mic_locations_size) 2
      signal_size = quot (fromIntegral signals_size) num_signals
      nx_native = fromIntegral nx
      ny_native = fromIntegral ny
      t_offset_native = fromIntegral t_offset
      t_duration_native = fromIntegral t_duration

  signals_flat <- (peekArray (fromIntegral signals_size) signals)
  mic_locations_flat
    <- (peekArray (fromIntegral mic_locations_size) mic_locations)

  let signals_native = map
        (map (realToFrac))
        (listToChannelData (map (realToFrac) signals_flat) num_signals)
      mic_locations_native_2D = map
        (map (realToFrac))
        (listToChannelData (map (realToFrac) mic_locations_flat) num_signals)
      mic_locations_native = listToCoordinates mic_locations_native_2D
      coordinates_native = accumulatedCorrelation
        signals_native
        mic_locations_native
        GridResolution2D { nx = nx_native, ny = ny_native }
        SampleWindow { offset = t_offset_native, duration =  t_duration_native }
      x = (realToFrac (xCoord2D coordinates_native)) :: CDouble
      y = (realToFrac (yCoord2D coordinates_native)) :: CDouble
  pokeArray coordinates [x, y]


-- foreign export stdcall accumulated_correlation
foreign export ccall accumulated_correlation
  :: Ptr CDouble -> CUInt -> Ptr CDouble -> CUInt -> CUInt -> CUInt ->
     CUInt -> CUInt -> Ptr CDouble -> IO ()
