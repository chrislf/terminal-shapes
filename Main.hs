{-# LANGUAGE MultiWayIf, BangPatterns #-}

module Main where

import Control.Monad
import System.IO
import qualified Data.List as List

newtype Tixel = Tixel { unTixel :: Char }

instance Semigroup Tixel where
  tx1@(Tixel px1) <> tx2 = if px1 == ' ' then tx2 else tx1

instance Monoid Tixel where
  mempty = Tixel ' '

newtype Pic a = Pic ((Int, Int) -> a)

instance Semigroup a => Semigroup (Pic a) where
  (Pic f1) <> (Pic f2) = Pic $ \p -> (f1 p) <> (f2 p)

instance Monoid a => Monoid (Pic a) where
  mempty = Pic $ \p -> mempty

instance Functor Pic where
  fmap f (Pic pf) = Pic $ f . pf

draw :: (Int, Int) -> Pic Tixel -> String
draw (w, h) (Pic pf) =
  List.intercalate
    ['\n']
    [[unTixel $ pf (x, y) | x <- [-hw..hw]] | y <- [-hh..hh]]
  where
    (hw, hh) = (w `div` 2, h `div` 2)

square :: Monoid a => a -> Int -> Pic a
square px n = Pic $ \(x, y) -> if abs x < n && abs y < n then px else mempty

poly :: a -> [(Int, Int)] -> Pic a
poly = undefined

circle :: Monoid a => a -> Int -> Pic a
circle px r = Pic $ \(x, y) -> if x * x + y * y < r * r then px else mempty

crop :: Monoid a => (Int, Int) -> Pic a -> Pic a
crop (w, h) (Pic f) = Pic $ \(x, y) ->
  if abs x < (w `div` 2) && abs y < (h `div` 2)
    then f (x, y)
    else mempty

scale :: (Int, Int) -> Pic a -> Pic a
scale (sx, sy) (Pic f) = Pic $ \(x, y) -> f (sx * x, sy * y)

translate :: (Int, Int) -> Pic a -> Pic a
translate (dx, dy) (Pic f) = Pic $ \(x, y) -> f (x - dx, y - dy)

window :: (Int, Int) -> Pic Tixel -> IO ()
window (w, h) pic = do
  -- attemping to enforce evaluation before writing, it does not work
  let !picStr = draw (w, h) $ picView <> frame
  putStr "\ESC[2J"  -- clear screen
  putStrLn picStr
  where
    picView  = translate (1, 1) . crop (w, h) $ pic
    frame    = translate (w `div` (-2), h `div` (-2)) $ Pic $ \pt@(x, y) -> Tixel $
      if | pt == (0, 0) -> '+' --'┌'
         | pt == (w - 1, 0) -> '+' --'┐'
         | pt == (0, h - 1) -> '+' --'└'
         | pt == (w - 1, h - 1) -> '+' --'┘'
         | (x == 0 || x == w - 1) && (0 < y && y < h - 1) -> '|' --'│'
         | (y == 0 || y == h - 1) && (0 < x && x < w - 1) -> '-' --'─'
         | otherwise -> ' '

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  aux (0, 0)
  where
    aux :: (Int, Int) -> IO ()
    aux (x, y) = do
      window (80, 25)
        . translate (x, y)
        $ mconcat [
            square (Tixel '#') 10
          -- , triangle 5
          , translate (20, 10) (circle (Tixel 'S') 8)
          ]
      c <- getChar
      case c of
        'w' -> aux (x, y + 1)
        'a' -> aux (x - 1, y)
        's' -> aux (x, y - 1)
        'd' -> aux (x + 1, y)
        'q' -> pure ()
        _   -> aux (x, y)
