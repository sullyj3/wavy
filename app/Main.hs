{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent ()
import Control.Monad ( replicateM )
import Control.Monad.IO.Class ( MonadIO )
import Data.Foldable ()
import qualified Data.List.NonEmpty as NE
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Class (normal, MonadSample)
import qualified Streamly.Internal.Data.Unfold as Unfold
import Streamly.Prelude (SerialT)
import qualified Streamly.Prelude as Stream
import Buttplug
import Buttplug.Message (Device, deviceName)
import qualified Buttplug.WebSockets as BPWS
import Control.Monad.IO.Class (liftIO)
import Ki.Unlifted (fork, scoped)
import qualified Data.Text.IO as T
import Control.Concurrent (threadDelay)
import Control.Monad
import System.Random

waitForEnter :: String -> IO ()
waitForEnter msg = do
  putStrLn msg
  () <$ getLine


main :: IO ()
main = do
  BPWS.runButtplugWebSockets "wavy" (BPWS.Connector "127.0.0.1" 12345) $ do
    addDeviceConnectedHandler $ \dev -> do
      liftIO $ T.putStrLn $ "device connected: " <> deviceName dev
    addDeviceConnectedHandler markov
    requestDeviceList
    _ <- startScanning
    liftIO $ waitForEnter "Scanning. Press enter to exit"
    _ <- stopAllDevices
    pure ()


pulse :: Device -> ButtplugM ()
pulse dev = forever do
  vibrateSingleMotor dev 0.1
  liftIO $ threadDelay 25000
  stopDevice dev
  waitTime <- randomRIO (30000, 2000000)
  liftIO $ threadDelay waitTime
  pure ()

data MarkovState = Short | Long


sleep n = threadDelay (n * 1000)

pulseSingleMotor dev speed millis = do
  vibrateSingleMotor dev speed
  liftIO $ sleep millis
  stopDevice dev


markov dev = loop Long
  where 
    loop s = do
      waitTime <- randomRIO $ case s of
        Short -> (50, 200)
        Long  -> (500, 5000)
      duration <- randomRIO $ case s of
        Short -> (50, 300)
        Long -> (50, 300)
      speed :: Double <- randomRIO $ case s of
        Short -> (0.1, 0.3)
        Long -> (0.7, 1)
      liftIO $ sleep waitTime
      pulseSingleMotor dev speed duration
      loop =<< liftIO (chooseNextState s)

    chooseNextState :: MarkovState -> IO MarkovState
    chooseNextState s = do
      x :: Float <- randomRIO (0, 1)
      pure $ if x > threshold
        then Short
        else Long
      where 
        threshold = case s of
          Short -> 0.05
          Long -> 0.6


wave :: Device -> ButtplugM ()
wave dev = do
  w <- liftIO . sampleIO $ waves
  Stream.mapM_ (vibrateSingleMotor dev) (funTime sampleRate w)
  where waves = ((bipolarToUnipolar . tanh . (* 10)) .) <$> chebfun 1 100
        sampleRate = 10

  

-- | ------------ | --
-- | Stream stuff | --
-- | ------------ | --

-- | Transforms a function of time (in seconds) into a stream of values emitted at the
-- given frequency
funTime ::
  MonadIO m =>
  Double ->        -- | The sample rate in Hz
  (Double -> a) -> -- | Function of time in seconds
  SerialT m a
funTime sampleRate f = Stream.map f 
                     . Stream.delay dt 
                     . Stream.unfold Unfold.enumerateFromStepNum $ (0, dt)
  where
    dt = 1 / sampleRate

-- | --------------- | --
-- | Numerical stuff | --
-- | --------------- | --

-- not sure if this is the right terminology - takes functions with range
-- [-1,1] to [0,1]
bipolarToUnipolar :: Double -> Double
bipolarToUnipolar x = (x / 2) + 0.5

-- periodic smooth random function
-- "Smooth Random Functions, Random ODEs, and Gaussian Processes"
-- - Silviu Filip, Aurya Javeed, Lloyd N. Trefethen
-- https://epubs.siam.org/doi/pdf/10.1137/17M1161853
chebfun :: forall m. MonadSample m => Double -> Double -> m (Double -> Double)
chebfun wavelength period
  | m <= 0 = const <$> distribution
  | otherwise = do
    -- these are nonempty, since m > 0
    as <- NE.fromList <$> mRandoms
    bs <- NE.fromList <$> mRandoms
    let a0 = NE.head as
        jsasbs = zip3 [1 ..] (NE.tail as) (NE.tail bs)

        f x = a0 + sqrt 2 * sum [term a b j | (j, a, b) <- jsasbs]
          where
            term a b j = a * cos theta + b * sin theta
              where
                theta = (2 * pi * j * x) / period
    pure f
  where
    m = floor (period / wavelength)
    mean = 0
    variance = 1 / (2 * fromIntegral m + 1)
    distribution = normal mean variance
    mRandoms = replicateM m distribution
