module Main where

import Control.Monad
import System.IO

square :: Int -> Pic
square n = Pic $ replicate n (replicate n '#')

triangle :: Int -> Pic
triangle = Pic . aux
  where
    aux :: Int -> [String]
    aux 0 = []
    aux n = replicate n 'T':aux (n-1)

circle :: Int -> Pic
circle r = Pic [ [ if x*x + y*y < r*r then 'S' else ' '
                 | x <- [-r..r]
                 ]
               | y <- [-r..r]
               ]

newtype Pic = Pic [String]

draw :: Pic -> IO ()
draw (Pic p) = do
  putStr "\ESC[2J"  -- clear screen
  -- forM_ p putStrLn -- really slow
  putStrLn $ unlines p -- also slow

-- Lines need to be padded on the right as well
-- This breaks the symmetry between the two functions
--  though: suspect it might be better done elsewhere
-- Right-padding is a waste of space: there isn't
--  anything to represent, except at the rendering
--  stage, where it can be done trivially (and flexibly)
overlay :: Pic -> Pic -> Pic
overlay (Pic p1) (Pic p2) = Pic $ zipWith combine pp1 pp2
  where
    maxHeight = max (length p1) (length p2)
    pad n ss = ss <> replicate n " "
    (pp1, pp2) = ( pad (maxHeight - length p1) p1
                 , pad (maxHeight - length p2) p2)
    combine = overlayLine

overlayLine :: String -> String -> String
overlayLine s1 s2 = zipWith combine p1 p2
  where
    pad n s = s <> replicate n ' '
    maxLen = max (length s1) (length s2)
    (p1, p2) = ( pad (maxLen - length s1) s1
               , pad (maxLen - length s2) s2)
    combine u o = if o == ' ' then u else o

-- isn't there a Functorish thing for this?
fm f (Pic p) = Pic (f p)

crop :: Int -> Int -> Pic -> Pic
crop w h = fm (take h . map (take w))

scale :: Int -> Int -> Pic -> Pic
scale = undefined -- TODO

translate :: Int -> Int -> Pic -> Pic
translate x y = fm $ shift " " y . map (shift ' ' x)
  where
    shift pad n
      | n > 0     = (replicate n pad <>)
      | otherwise = drop (negate n)

instance Semigroup Pic where
  (<>) = overlay

instance Monoid Pic where
  mempty = Pic []

window :: Int -> Int -> Pic -> IO ()
window x y p = draw $ picView <> frame
  where
    surround e c = (e:c) ++ [e]
    headTail = surround '+' $ replicate x '-'
    blank    = replicate y $ surround '|' $ replicate x ' '
    frame    = Pic . surround headTail $ blank
    picView  = translate 1 1 . crop x y $ p

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  aux 0 0
  where
    aux x y = do
      window 80 25
        . translate x y
        $ mconcat [
            square 10
          , triangle 5
          , translate 20 10 (circle 8)
          ]
      c <- getChar
      case c of
        'w' -> aux x (y + 1)
        'a' -> aux (x - 1) y
        's' -> aux x (y - 1)
        'd' -> aux (x + 1) y
        'q' -> pure ()
        _   -> aux x y

