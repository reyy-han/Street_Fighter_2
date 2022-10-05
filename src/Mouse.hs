module Mouse where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

type Mouse = Set MouseButton

createMouse :: Mouse
createMouse = S.empty

handleEvent :: Event -> Mouse -> Mouse
handleEvent event mb =
  case eventPayload event of
    MouseButtonEvent mousebuttonevent ->
      if mouseButtonEventMotion mousebuttonevent == Pressed
      then S.insert (mouseButtonEventButton mousebuttonevent) mb
      else if mouseButtonEventMotion mousebuttonevent == Released
           then S.delete (mouseButtonEventButton mousebuttonevent) mb
           else mb
    _ -> mb

handleEvents :: [Event] -> Mouse -> Mouse
handleEvents events mb = foldl' (flip handleEvent) mb events

mousePressed :: MouseButton -> Mouse -> Bool
mousePressed b mb = S.member b mb

-- | Regarder si mauvais
mousepressed :: Event -> Bool
mousepressed event = 
  case eventPayload event of
    MouseButtonEvent mousebuttonevent ->
        if mouseButtonEventMotion mousebuttonevent == Pressed
        then True
        else False
    _ -> False