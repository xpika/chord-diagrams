{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Typeable
import Music.Diatonic

tt t = (text (t) # scale 0.01 # fc white )
dot c = ( tt c `atop` ((circle 0.02 # fc green) # centerY # lw 0.001))
vdots q = foldr1 (===)  (zipWith ($) (replicate 4 dot) q)
dotGrid = foldr1 (|||) (map vdots ( map (map show) (frettedGuitarStrings standardTuning) ))

frettedGuitarStrings tuning = map fret tuning
fret tune = map (\n -> canonize . applyNTimes sharp n $ tune) [0..]

applyNTimes f n x = iterate f x !! n

standardTuning = [E,A,D,G,B,E]

liveMain = do
           print 3
           renderSVG "file.svg" (Width 1000) (dotGrid :: Diagram B R2)    