{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent ()
import Control.Concurrent.STM.TVar
    ( newTVarIO, readTVarIO, writeTVar )
import Control.Monad ( replicateM )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.STM ( atomically )
import Data.Foldable ()
import Data.Function (on)
import qualified Data.List.NonEmpty as NE
-- import Data.RVar ( runRVar, sampleRVar, MonadRandom, RVar )
-- import Data.Random ( normal, StdRandom(StdRandom) )
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Class (normal, MonadSample)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Motif ( (.>), (|>) )
import qualified Streamly.Internal.Data.Unfold as Unfold
import Streamly.Prelude (Serial, SerialT)
import qualified Streamly.Prelude as Stream
import qualified System.Console.Terminal.Size as Term
import System.Posix.Signals ( installHandler, Handler(Catch) )
import System.Posix.Signals.Exts ( sigWINCH )
import Buttplug
import qualified Buttplug.WebSockets as BPWS
import Control.Monad.IO.Class (liftIO)
import Ki.Unlifted (fork, scoped)

main :: IO ()
main = do
  BPWS.runButtplugWebSockets "wavy" (BPWS.Connector "localhost" 12345) $ do
    startScanning
    liftIO $ do
      putStrLn "Scanning"
      getLine

    requestDeviceList >>= \case
      [] -> do
        liftIO $ putStrLn "No devices found"
      (d1:_) -> scoped $ \scope -> do
        w <- liftIO . sampleIO $ waves
        fork scope $ Stream.mapM_ (vibrateSingleMotor d1) (funTime sampleRate w)
        liftIO $ do
          putStrLn "press enter to exit"
          () <$ getLine
  
  where waves = ((bipolarToUnipolar . tanh . (* 10)) .) <$> chebfun 1 100
        sampleRate = 10

-- | ------------ | --
-- | Stream stuff | --
-- | ------------ | --

-- | Transforms a function of time (in seconds) into a stream of values emitted at the
-- given frequency
funTime ::
  MonadIO m =>
  -- | The sample rate in Hz
  Double ->
  -- | Function of time in seconds
  (Double -> a) ->
  SerialT m a
funTime sampleRate f =
  Stream.unfold (Unfold.enumerateFromStepNum dt) 0
    |> Stream.delay dt
    |> Stream.map f
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
  | m <= 0 = do
    a0 <- distribution
    pure $ const a0
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
