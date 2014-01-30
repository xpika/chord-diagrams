{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Typeable
import Graphics.SVGFonts.ReadFont (textSVG)
import Music.Instrument.Chord
import Control.Monad


whiteKey = rect 2 2 # fc white

main = do renderSVG "piano.svg" (Absolute) (  (\x -> (hcat (replicate 3 x))) ((liftM2 (|||))
            (\s -> s (s ((whiteKey # alignT  )) ))
            (\s -> s (s ( s(whiteKey# alignT  )) ))
            ( \ f ->
                ( strutX 1 ||| rect 1 1# fc grey # alignT ) 
                `atop`
                ((whiteKey# alignT  ) ||| f)
            )) # scaleY 5 # scale 20 # lw 2  :: Diagram B R2) 