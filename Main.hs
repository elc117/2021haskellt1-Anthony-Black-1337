--CODIGO PRINCIPAL

import Text.Printf

--Tipos Criados 

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)
type Ellipse   = (Point,Float,Float)
type Line      = (Point,Point)
--type Polygon   = --???

-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paletas com n valores retirados de uma lista com sequências de R, G e B 

--Retangulos
rgbPaletteRect :: Int -> [(Int,Int,Int)]
rgbPaletteRect n = take n $ cycle [(28,28,28),(79,79,79),(128,128,128)] 
--Circulos
rgbPaletteCircle :: Int -> [(Int,Int,Int)]
rgbPaletteCircle n = take n $ cycle [(21,42,121),(2,135,139),(128,9,107)]
--Ellipses
rgbPaletteEllipse :: Int -> [(Int,Int,Int)]
rgbPaletteEllipse n = take n $ cycle [(255,18,24),(0,255,100),(242,79,0)]
--Linhas
rgbPaletteLine :: Int -> [(Int,Int,Int)]
rgbPaletteLine n = take n $ cycle [(255,0,0),(0,255,127),(255,0,127)]

-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 10),w,h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,1000)
        gap = 0
-------------------------------------------------------------------------------
-- Geração de Circulos em suas posições
-------------------------------------------------------------------------------

genCircleInLine :: Int -> [Circle]
genCircleInLine n  = [((o*(gapp+2*r) + r*2, gapp+50*o),gapp+r*o) | o <- [0..fromIntegral (n-1)]]
  where r = 10
        gapp = 20

-------------------------------------------------------------------------------
-- Geração de Ellipses em suas posições
-------------------------------------------------------------------------------
genEllipseInLine :: Int -> [Ellipse]
genEllipseInLine n  = [((p*(gappp+5*rx) + rx*2, 750),rx/p,ry*p+2) | p <- [0..fromIntegral (n-1)]]
  where rx = 20
        ry = 30
        gappp = 5
-------------------------------------------------------------------------------
-- Geração de Linhas em suas posições
-------------------------------------------------------------------------------
genLineInLine :: Int -> [Line]
genLineInLine n  = [((q*40*gapppp, 10),(x2, y2)) | q <- [0..fromIntegral (n-1)]]
  where x2 = 2500/2
        y2 = 1000/2
        gapppp = 10 

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------
-- Gera string representando retângulo, circulo e ellipse SVG 

svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect  x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

svgCircle :: Circle -> String -> String 
svgCircle ((cx,cy),r) style = 
  printf "<circle  cx='%.3f' cy='%.3f' r='%.3f' fill='%s' />\n" cx cy r style

svgEllipse :: Ellipse -> String -> String 
svgEllipse ((cx,cy),rx,ry) style = 
  printf "<ellipse  cx='%.3f' cy='%.3f' rx='%.3f' ry='%.3f' fill='%s' />\n" cx cy rx ry style

svgLine :: Line -> String -> String
svgLine ((x1,y1),(x2,y2)) style =
  printf "<line x1=\"%.3f\" y1=\"%.3f\" x2=\"%.3f\" y2=\"%.3f\" style=\"stroke-width:10;stroke:%s\"/>\n" x1 y1 x2 y2 style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor

--Cor do Retangulo

svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

--Cor do circulo

svgCircleStyle :: (Int,Int,Int) -> String
svgCircleStyle (r,g,b) = printf "rgb(%d,%d,%d)" r g b

--Cor da ellipse

svgEllipseStyle :: (Int,Int,Int) -> String
svgEllipseStyle (r,g,b) = printf "rgb(%d,%d,%d)" r g b

-- Cor da linha
svgLineStyle :: (Int,Int,Int) -> String
svgLineStyle (r,g,b) = printf "rgb(%d,%d,%d)" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo

svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles


-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "main.svg" $ svgstrs
  where svgstrs = svgBegin w h  ++ svgfigs ++ svgLineX ++ svgCircleX ++ svgEllipseX  ++ svgEnd

        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInLine nrects
        palette = rgbPaletteRect nrects
        nrects = 48
        -----------------------------------------------------------------------
        svgCircleX = svgElements svgCircle circle (map svgCircleStyle palheta)
        circle = genCircleInLine nCircle 
        palheta = rgbPaletteCircle nCircle
        nCircle = 10
        -----------------------------------------------------------------------
        svgEllipseX= svgElements svgEllipse ellipse (map svgEllipseStyle pallheta)
        ellipse = genEllipseInLine nEllipse 
        pallheta = rgbPaletteEllipse nEllipse
        nEllipse = 10
        -----------------------------------------------------------------------
        svgLineX= svgElements svgLine line (map svgLineStyle palllheta)
        line = genLineInLine nLine 
        palllheta = rgbPaletteLine nLine
        nLine = 7
        -----------------------------------------------------------------------
        (w,h) = (2500,1000) -- Largura e Altura da imagem SVG (minha tela de desenho)
       