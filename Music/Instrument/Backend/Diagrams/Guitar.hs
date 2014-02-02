{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Typeable
import Graphics.SVGFonts.ReadFont (textSVG)
import Music.Instrument.Chord

noteText t = textSVG t 0.5 #  stroke # fc white # lc white

dot c = ( 
          (===)    (noteText c  
          <>  circle 0.3 # fc black # lc purple # lw 0.02 # centerY 
          <>  vrule 1  # lc white # lw 0.02
          <>  rect 1 1 # fc orange  # lc orange # lw 0.01) 
          (hrule 1 # scale 1 #  fc grey )
        ) 
        
vdots (q,i) = vcat (take 5 (map (\(x,y) -> named ((y::Int,i::Int)) (dot (show (y,i)) )) (zip q [0..]) ))
dotGrid = foldr1 (|||) (map vdots (zip ( map (map show) (frettedGuitarStrings standardTuning) ) [0..]) )

frettedGuitarStrings tuning = map fret tuning
fret tune = map (\n -> canonize . applyNTimes sharp n $ tune) [0..]

applyNTimes f n x = iterate f x !! n

main = do
       renderSVG "guitar.svg" (Width 1000) ( (withName (1::Int,2::Int) (const (circle 1 `atop`)) dotGrid ) :: Diagram B R2)
       (print (take 1 $ findPositionPatterns (minorChord B) standardTuning 4))