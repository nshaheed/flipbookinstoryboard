{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Storyboard.Active() -- for the instance

import Graphics.Blank             as B
import Graphics.Storyboard        as SB
import Graphics.Flipbook.Effects

import qualified Data.Text as Text
import           Data.Text (Text)
import Data.Active

main :: IO ()
main = SB.storyBoard [ slide ]

--A quick and dirty example of flipbooks fadein working in storyboard

textFade :: Active (Canvas ())
textFade = let f   = Text.pack "\\x -> x + 2"
               fName = "f"
               arg = Text.unwords ["let",fName,"=",""]
               txt = combineDraw arg f (50,50)
           in fadein 0.5 txt

-- | Takes two Text strings and places y immediately after x.
--   returns the width of the first string and the width of both
--   strings combined.
--   (taken from Main.hs of flipbook)
combineDraw :: Text -> Text -> (Float, Float) -> Canvas ()
combineDraw a b (x,y) = B.saveRestore $ do
      B.font "10pt Calibri"
      TextMetrics wa <- B.measureText a
      TextMetrics _  <- B.measureText b
      B.translate (x, y)
      B.fillText (a, 0, 0)
      B.fillText (b, wa, 0)
                                                      
               

slide :: SB.Slide ()
slide = do
      SB.align SB.center $ SB.p $ "Active Plugin Example"

      -- let t = SB.drawMovieTile (200,200) $ mkActive 0 10 $ \ tm -> do
      --             beginPath()
      --             rect(1,1,20 * fromTime tm,20 * fromTime tm)
      --             strokeStyle "red"
      --             lineWidth 1
      --             closePath()
      --             stroke()

      let t = SB.drawMovieTile (200,200) $ stretch 10 textFade

      SB.place SB.top (SB.nudge SB.center SB.center t)

      return ()
