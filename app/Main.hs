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

import System.Posix.Signals
import System.Posix.Signals.Exts
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Motif

getWidth :: IO Int
getWidth = do
  Just Term.Window {Term.width=width} <- Term.size
  pure width

main :: IO ()
main = do
  initialWidth <- getWidth

  widthVar <- newTVarIO initialWidth

  let winCHHandler :: IO ()
      winCHHandler = do
        width <- getWidth
        atomically $ writeTVar widthVar width

  installHandler sigWINCH (Catch winCHHandler) Nothing

  let f1 = sinusoid 1 0.3 0 0
      f2 = sinusoid 0.1 3 2 0
      f3 = sinusoid 0.2 4 2.4 0
      f4 = sinusoid 0.4 7 2.4 0
      fSum x = f1 x + f2 x + f3 x + f4 x

      f = bipolarToUnipolar . tanh . fSum

  for_ [0.01, 0.02 ..] \x -> do
    width <- readTVarIO widthVar
    T.putStrLn $ nth width (quantize width . f $ x) '.'
    sleep 0.01

sinusoid a b h k x = a * sin (b * (x-h)) + k

-- take a function with range [0,1], quantize it and scale it to [0,steps)
quantize :: Int -> Double -> Int
quantize steps x = round $ (fromIntegral (steps-1)) * x

nth :: Int -> Int -> Char -> Text
nth width n c
  | n >= width = T.replicate width " "
  | otherwise  = T.replicate n " " <> T.singleton c <> T.replicate (width - n - 1) " "

-- not sure if this is the right terminology - takes functions with range
-- [-1,1] to [0,1]
bipolarToUnipolar :: Double -> Double
bipolarToUnipolar x = (x / 2) + 0.5
