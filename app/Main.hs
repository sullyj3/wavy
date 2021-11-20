{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Foldable
import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import Data.RVar
import Data.Random
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Motif
import qualified System.Console.Terminal.Size as Term
import System.Posix.Signals
import System.Posix.Signals.Exts

getWidth :: IO Int
getWidth = do
  Just Term.Window {Term.width = width} <- Term.size
  pure width

main :: IO ()
main = do
  widthVar <- newTVarIO =<< getWidth
  let winCHHandler = atomically . writeTVar widthVar =<< getWidth
  installHandler sigWINCH (Catch winCHHandler) Nothing

  randFun <- chebfun 0.3 100
  let f = bipolarToUnipolar . tanh . (* 10) . randFun
      delta = 0.005

  for_ [0, delta ..] \x -> do
    width <- readTVarIO widthVar
    let c = case floatCompare 0.003 (f (x-delta)) (f x) of
          LT -> '\\'
          EQ -> '|'
          GT -> '/'
    T.putStrLn $ charAtNth width (quantize width . f $ x) c
    sleep 0.01
  where
    charAtNth :: Int -> Int -> Char -> Text
    charAtNth width n c
      | n >= width = T.replicate width " "
      | otherwise = T.replicate n " " <> T.singleton c <> T.replicate (width - n - 1) " "

-- | --------------- | --
-- | Numerical stuff | --
-- | --------------- | --

floatCompare epsilon a b
  | abs (a - b) < epsilon = EQ
  | otherwise = compare a b

sinusoid a b h k x = a * sin (b * (x - h)) + k

-- take a function with range [0,1], quantize it and scale it to [0,steps)
quantize :: Int -> Double -> Int
quantize steps x = round $ (fromIntegral (steps -1)) * x


-- not sure if this is the right terminology - takes functions with range
-- [-1,1] to [0,1]
bipolarToUnipolar :: Double -> Double
bipolarToUnipolar x = (x / 2) + 0.5

-- periodic smooth random function
-- "Smooth Random Functions, Random ODEs, and Gaussian Processes"
-- - Silviu Filip, Aurya Javeed, Lloyd N. Trefethen
-- https://epubs.siam.org/doi/pdf/10.1137/17M1161853
chebfun :: MonadRandom m => Double -> Double -> m (Double -> Double)
chebfun wavelength period
  | m <= 0 = do
    a0 <- runRVar distribution StdRandom
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
    m :: Int
    m = floor (period / wavelength)
    mean = 0
    variance :: Double
    variance = 1 / (2 * fromIntegral m + 1)

    distribution :: RVar Double
    distribution = normal mean variance

    mRandoms :: MonadRandom m => m [Double]
    mRandoms = sampleRVar $ replicateM m distribution
