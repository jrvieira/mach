module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

w, h :: Float
w = 1000
h = 400

main :: IO ()
main = play (InWindow "mach sim" (round w,round h) (0, 0)) (makeColorI 0 0 0 0) 16 state render catch step

data State = State { frame :: Float , run :: Run , objects :: [Object] }
data Run = Pred | Zero | Succ deriving Eq
data Object = Plane { ι :: String , ν :: Bool , κ :: Color , μ :: Float } | Boom Color Path | Floor Color Float | Wall Color Float | Beam Float
--                                    ^mach speed               ^y

state :: State
state = State
   { frame = 0
   , run = Zero
   , objects =
      [ Wall (makeColorI 255 255 255 30) (w/2)
   -- , Plane (makeColorI 255 255 255 75) 1.001
      , Plane "P1" True (makeColorI 255 0 255 255) 1.0
      , Plane "P2" True (makeColorI 0 255 255 255) 1.25
      , Plane "P3" True (makeColorI 255 255 0 255) 2
   -- , Beam 100
      , Boom (makeColorI 70 70 70 255) []
   -- , Floor (makeColorI 255 255 255 30) -100
      ]
   }

render :: State -> Picture
render s = translate (negate $ w / 2) 0 $ Pictures $ map draw (objects s)
   where
   draw :: Object -> Picture
   draw (Plane _ True k m) = Pictures $ label : map sound (drop (round $ frame s - w/2) [0,3..frame s])
      where
      x = (w/4) * (2 - m)
      label = translate (frame s * m + x) 100 $ color k $ scale 0.07 0.07 $ text (show m)
      sound :: Float -> Picture
      sound f = translate (m * f + x) 0 $ color (withAlpha (1 / ((frame s - f) / 2)) k) $ circle (frame s - f)
   draw (Boom k ps) = color k $ line ps
   draw (Floor k y) = color k $ line [(0,y),(w,y)]
   draw (Wall k x) = color k $ line [(x,-h),(x,h)]
   draw (Beam x) = translate x 0 $ color (makeColorI 255 255 255 255) $ circle (frame s)
   draw _ = Blank

catch :: Event -> State -> State
catch (EventKey k ks mo (_x,_y)) s
   | Char '0' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { frame = w/4 }
-- | Char ' ' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { run = if run s /= Succ then Succ else Zero }  -- :(
   | Char 'h' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { run = Pred }
   | Char 'h' <- k , Up   <- ks , Modifiers _ Up Up <- mo = s { run = Zero }
   | Char 'H' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { run = Pred }
   | Char 'l' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { run = Succ }
   | Char 'l' <- k , Up   <- ks , Modifiers _ Up Up <- mo = s { run = Zero }
   | Char 'L' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { run = Succ }
   | Char 'k' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { objects = speed (- 0.01) <$> objects s }
   | Char 'j' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { objects = speed (+ 0.01) <$> objects s }
   | Char 'K' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { objects = speed (- 0.1) <$> objects s }
   | Char 'J' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { objects = speed (+ 0.1) <$> objects s }
   | Char '1' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { objects = toggle "P1" <$> objects s }
   | Char '2' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { objects = toggle "P2" <$> objects s }
   | Char '3' <- k , Down <- ks , Modifiers _ Up Up <- mo = s { objects = toggle "P3" <$> objects s }
   | otherwise = s
catch _ st = st

toggle :: String -> Object -> Object
toggle x o@Plane {}
   | x == ι o = o { ν = not (ν o) }
toggle _ o = o

speed :: (Float -> Float) -> Object -> Object
speed f (Plane i v k m) = Plane i v k (f m)
speed _ o = o

step :: Float -> State -> State
step _ s
   | Zero <- run s = s
   | Pred <- run s = s { frame = pred $ frame s , objects = objects' }
   | Succ <- run s = s { frame = succ $ frame s , objects = objects' }
   where
   objects' = map m $ filter f $ objects s
   f = const True
   m = id
