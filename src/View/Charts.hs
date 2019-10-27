{-# LANGUAGE OverloadedStrings          #-}

module View.Charts where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time
import Data.Colour.SRGB (sRGB24)

rootPath :: String
rootPath = "static\\img\\"

fileOptions :: FileOptions
fileOptions = FileOptions (600,600) SVG loadSansSerifFonts

circlePart :: (String, Double, Bool) -> PieItem
circlePart (s,v,needOffset) = pitem_value .~ (v+1)
                            $ pitem_label .~ s
                            $ pitem_offset .~ (if needOffset then 10 else 0)
                            $ def

eraseSpace :: String -> String
eraseSpace str = filter isNotSpace str
  where isNotSpace ch = (ch /= ' ')

beautyRed :: (Ord a, Floating a) => Colour a
beautyRed = sRGB24 252 106 123

beautyYellow :: (Ord a, Floating a) => Colour a
beautyYellow = sRGB24 255 184 108

beautyBlue :: (Ord a, Floating a) => Colour a
beautyBlue = sRGB24 101 192 227

beautyGreen :: (Ord a, Floating a) => Colour a
beautyGreen = sRGB24 132 243 103

beautyBlack :: (Ord a, Floating a) => Colour a
beautyBlack = sRGB24 116 106 128

createCircleDiagram :: String -> String -> [ (String,Double,Bool) ] -> IO ()
createCircleDiagram unicId title values = 
  let filePath = rootPath ++ unicId ++ (eraseSpace title) ++ ".svg"
  in
    toFile fileOptions filePath $ do
      pie_title .= title
      pie_margin .= 40
      pie_plot . pie_colors .= cycle (map opaque [beautyRed, beautyYellow, beautyBlue, beautyGreen])
      pie_plot . pie_data .= map circlePart values
     
createCrackChart :: String -> String -> [(UTCTime,Double,Double)] -> IO ()
createCrackChart unicId title values =
  let filePath = rootPath ++ unicId ++ (eraseSpace title) ++ ".svg"
  in     
    toFile fileOptions filePath $ do
      layoutlr_title .= title
      layoutlr_margin .= 40
      layoutlr_left_axis . laxis_override .= axisGridHide
      layoutlr_right_axis . laxis_override .= axisGridHide
      plotLeft $ do liftEC $ do
                            plot_lines_title .= "Incomes"
                            plot_lines_values .= [ [ (d,v) | (d,v,_) <- values] ]
                            plot_lines_style . line_color .= opaque beautyGreen
      plotRight $ do liftEC $ do
                                  plot_lines_title .= "Expenses"
                                  plot_lines_values .= [ [ (d,v) | (d,_,v) <- values] ]
                                  plot_lines_style . line_color .= opaque beautyRed                     