{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}
import           Control.Concurrent
import           Control.Monad
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable.Mutable as MV
import           Linear                       (V2 (..))
import           Options.Applicative
import           System.PortAudio


-- TODO:
-- Create an interpolation algorithm to map one table onto another (up/downsample)

sr :: Double 
sr = 44100

freqToSamps :: Double -> Int
freqToSamps freq =
  round $ sr / freq

-- Set the periods
period :: Int
period = freqToSamps 440


-- Create tables

table :: V.Vector Float
table = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]



go :: Int -> Int -> Int -> MV.IOVector (V2 Float) -> IO ()
go i0 i n o
  | i == n = return ()
  | otherwise = do
    let v = table V.! ((i0 + i) `mod` period)
    MV.write o i (V2 v v)
    go i0 (i + 1) n o

-- The main audio calback

callback :: MVar Int -> Status -> input -> MV.IOVector (V2 Float) -> IO StreamCallbackResult
callback phase _ _ o = do
  let n = MV.length o
  i0 <- takeMVar phase
  go i0 0 n o
  putMVar phase $ i0 + n
  return Continue


-- IO stuff

app :: Parser (IO ())
app = do
  rate <- option auto $ long "rate" <> help "sampling rate" <> value 44100
  buf <- option auto $ long "buffer" <> help "number of samples for the buffer" <> value 64
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
          withStartStream s $ threadDelay $ 1000 * 10000


main :: IO ()
main = join $ execParser (info app mempty)



