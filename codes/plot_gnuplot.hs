{-
From: http://rosettacode.org/wiki/Plot_coordinate_pairs#Haskell

Plot coordinate pairs

Plot a function represented as `x', `y' numerical arrays.
Post link to your resulting image for input arrays (see Example section for Python language on Query Performance page)

x  = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
y = {2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0 

 -}


import Graphics.Gnuplot.Simple
 
pnts = [2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0]
 
doPlot = plotPathStyle [ ( Title "plotting dots" )]
            (PlotStyle Points (CustomStyle []))  (zip [0..] pnts)
