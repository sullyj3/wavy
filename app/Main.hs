{-# language BlockArguments, ScopedTypeVariables,
 OverloadedStrings, NumericUnderscores #-}

module Main where

import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import Control.Concurrent
import Control.Monad
import qualified System.Console.Terminal.Size as Term
import Data.Function (on)

-- instance Num a => Num (r -> a) where
--   f1 + f2 = \r -> f1 r + f2 r
--   f1 * f2 = \r -> f1 r * f2 r
--   abs f = abs . f
--   signum f = signum . f
--   fromInteger i = const (fromIntegral i)
--   negate f = negate . f

-- instance Fractional a => Fractional (r -> a) where
--   fromRational r = const $ fromRational r
--   f1 / f2 = \r -> f1 r / f2 r

main :: IO ()
main = do
  Just Term.Window {Term.width=width} <- Term.size

  let f1 = sinusoid 1 0.3 0 0
      f2 = sinusoid 0.1 3 2 0
      f3 = sinusoid 0.2 4 2.4 0
      f4 = sinusoid 0.4 7 2.4 0
      fSum x = f1 x + f2 x + f3 x + f4 x

  for_ [0.01, 0.02 ..] \x -> do
    T.putStrLn $ nth width (quantize width fSum x) '.'
    sleep 0.01

sleep :: Double -> IO ()
sleep seconds = threadDelay $ round (seconds * 1_000_000)

sinusoid a b h k x = a * sin (b * (x-h)) + k

-- >>> sin (pi/2)
-- 1.0

-- >>> quantize 10 sin (pi/2)
-- 9

-- >>> sin (3*pi/2)
-- -1.0

-- >>> quantize 10 sin (3*pi/2)
-- 0

-- take a function with range [-1,1], quantize it and scale it to [0,steps)
quantize :: Int -> (Double -> Double) -> (Double -> Int)
quantize steps f x = round $ (fromIntegral (steps-1)/2) * (f x + 1.0)

nth :: Int -> Int -> Char -> Text
nth width n c
  | n >= width = T.replicate width " "
  | otherwise  = T.replicate n " " <> T.singleton c <> T.replicate (width - n - 1) " "

