-- http://elm-lang.org/examples/forward-apply

module ForwardApply where

import Color exposing (green, purple)
import Graphics.Collage exposing (collage, filled, move, ngon)

purplePentagon =
  move (20,20) (filled purple (ngon 5 50))

greenPentagon =
  ngon 5 50
  |> filled green
  |> move (-20,-20)

{-----------------------------------------------------------------
   Exercises:
 -----------------------------------------------------------------}

purplePentagon' =
  ngon 5 50
  |> filled purple
  |> move (20,20)

greenPentagon' =
  move (-20, -20) (filled green (ngon 5 50))
