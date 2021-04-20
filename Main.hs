{-# LANGUAGE ViewPatterns, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
import System.PortAudio
import Control.Concurrent
import Control.Monad
import Linear (V2(..))
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MV
import Options.Applicative

-- Set the periods
period :: Int
period = 100

period2 :: Int
period2 = 4410

period3 :: Int
period3 = 8000

period4 :: Int
period4 = 2000

-- Create tables

table :: V.Vector Float
table = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]

table2 :: V.Vector Float
table2 = V.fromList [sin t | i <- [0..period2 - 1], let t = fromIntegral i / fromIntegral period2 * 2 * pi]

table3 :: V.Vector Float
table3 = V.fromList [sin t | i <- [0..period3 - 1], let t = fromIntegral i / fromIntegral period3 * 2 * pi]

table4 :: V.Vector Float
table4 = V.fromList [sin t | i <- [0..period4 - 1], let t = fromIntegral i / fromIntegral period4 * 2 * pi]

callback :: MVar Int -> Status -> input -> MV.IOVector (V2 Float) -> IO StreamCallbackResult
callback phase _ _ o = do
  i0 <- takeMVar phase
  go i0 0
  putMVar phase $ i0 + n
  return Continue
  where
    n = MV.length o
    go :: Int -> Int -> IO ()
    go i0 i
      | i == n = return ()
      | otherwise = do
        let v = table V.! ((i0 + i) `mod` period)
        let v2 = table2 V.! ((i0 + i) `mod` period2)
        let v3 = table3 V.! ((i0 + i) `mod` period3)
        let v4 = table4 V.! ((i0 + i) `mod` period4)
        -- Write 2 channels
        let merge = v + v2 + v3 + v4
        MV.write o i (V2 (v + v2 + v3 + v4) (v + v2 + v3 + v4))
        go i0 (i + 1)

app :: Parser (IO ())
app = do
  rate <- option auto $ long "rate" <> help "sampling rate" <> value 44100
  buf <- option auto $ long "buffer" <> help "number of samples for the buffer" <> value 1024
  device <- option auto $ long "device" <> help "device index" <> value (-1)
  pure $ withPortAudio $ do
    phase <- newMVar 0
    (_, devs) <- getDevices
    do
      putStrLn "–––––––––––"
      putStrLn $ "Sample rate: " ++ show rate ++ "hz"
      putStrLn $ "buffer size: " ++ show buf ++ " samples"
      putStrLn "–––––––––––"
      putStrLn "Please choose a decvice:"
      forM_ (zip [0 :: Int ..] devs) $ \(i, dev) -> 
        putStrLn $ show i ++ ": " ++ deviceName dev
      devI <- getLine
      let dev = devs !! read devI
      let output = streamParameters dev 0
      withStream rate buf noConnection output mempty (callback phase)
        $ \s -> do
          setStreamFinishedCallback s $ putStrLn "Done"
          withStartStream s $ threadDelay $ 1000 * 1000


main :: IO ()
main = join $ execParser (info app mempty)


            
