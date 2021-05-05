{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
import           Control.Concurrent
import GHC.Float
import           Control.Monad
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Linear                       (V2 (..))
import           Options.Applicative
import           System.PortAudio


-- TODO:
-- Create an interpolation algorithm to map one table onto another (up/downsample)

sr :: Double
sr = 22050

bufferSize :: Int
bufferSize = 128

freqToSamps :: Double -> Int
freqToSamps freq =
  round $ sr / freq

-- Set the frequeency

period :: Int
period = freqToSamps 440

-- Create tables

table :: V.Vector Float
table = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]

-- Define signal funcions

sineTable :: Int -> Int -> Float
sineTable buf tick =
  table V.! ((buf + tick) `mod` period)

accum :: Float -> Int -> Int -> Float
accum freq buf tick =
  let
    inc =  1 / (double2Float sr / freq)
  in
    int2Float ((buf + tick) `mod` freqToSamps (float2Double freq)) * inc



-- The main audio callback
-- This gets called each buffer cycle and contains a function that fills the output buffer each cycle

callback :: MVar Int -> Status -> input -> MV.IOVector (V2 Float) -> IO StreamCallbackResult
callback phase _ input output = do
  -- Get the length of the output buffer
  let n = MV.length output
  -- Get the phase
  i0 <- takeMVar phase
  -- Write values into the output buffer using a signal function staturting at 0
  go i0 0 n output
  -- incriment the phase by the buffer size
  putMVar phase $ i0 + n
  -- Stream callback result
  return Continue
  where
    go :: Int -> Int -> Int -> MV.IOVector (V2 Float) -> IO ()
    go buffCount tick buffLength outBuff
      | tick == buffLength = return ()
      | otherwise = do
        let v = accum 100 buffCount tick
        print v
        MV.write outBuff tick (V2 v v)
        go buffCount (tick + 1) buffLength outBuff


-- IO stuff

app :: Parser (IO ())
app = do
  rate <- option auto $ long "rate" <> help "sampling rate" <> value sr
  buf <- option auto $ long "buffer" <> help "number of samples for the buffer" <> value bufferSize
  device <- option auto $ long "device" <> help "device index" <> value (-1)
  pure $ withPortAudio $ do
    phase <- newMVar 0
    (inDevs, outDevs) <- getDevices
    do
      putStrLn "–––––––––––"
      putStrLn $ "Sample rate: " ++ show rate ++ "hz"
      putStrLn $ "buffer size: " ++ show buf ++ " samples"
      putStrLn "–––––––––––"

      -- Retrive all input devices and manually select one

      -- putStrLn "Please choose an input decvice:"
      -- forM_ (zip [0 :: Int ..] inDevs) $ \(i, dev) ->
      --   putStrLn $ show i ++ ": " ++ deviceName dev
      -- devI <- getLine
      -- let inDev = inDevs !! read devI
      -- let input = streamParameters inDev 0

      -- Retrive all output devices and manually select one

      putStrLn "Please choose an output decvice:"
      forM_ (zip [0 :: Int ..] outDevs) $ \(i, dev) ->
        putStrLn $ show i ++ ": " ++ deviceName dev
      devO <- getLine
      let outDev = outDevs !! read devO
      let output = streamParameters outDev 0


      withStream rate buf noConnection  output mempty (callback phase)
        $ \s -> do
          setStreamFinishedCallback s $ putStrLn "Done"
          withStartStream s $ threadDelay $ 1000 * 10000


main :: IO ()
main = join $ execParser (info app mempty)

