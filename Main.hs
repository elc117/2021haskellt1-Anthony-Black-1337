 --CODIGO PRINCIPAL

import Text.Printf

-------------------------------------------------------------------------------
--Tipos Criados 
-------------------------------------------------------------------------------
type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)
type Ellipse   = (Point,Float,Float)
type Line      = (Point,Point)
type Polygon   = (Point,Point,Point)
-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paletas com n valores retirados de uma lista com sequências de R, G e B 

--Retangulos
rgbPaletteRect :: Int -> [(Int,Int,Int)]
rgbPaletteRect n = [(i,i,i) | i <- [0..n] ] 

--Circulos
rgbPaletteCircle :: Int -> [(Int,Int,Int)]
rgbPaletteCircle n = take n $ cycle [(11+r,32+r,111+r),(g,133+g,137+g),(100+b,-21+b,73+b)]
      where r = 10
            g = 2
            b = 30

--Ellipses
rgbPaletteEllipse :: Int -> [(Int,Int,Int)]
rgbPaletteEllipse n = take n $ cycle [(245+r,8+r,14+r),(g,253+g,98+g),(212+b,49+b,b)]
      where r = 10
            g = 2
            b = 30

--Linhas
rgbPaletteLine :: Int -> [(Int,Int,Int)]
rgbPaletteLine n = take n $ cycle [(255,255,255),(255-n*2,0+n*2,0+n*4),(0+n*4,0+n*2,255-n*2)]               
            
--Trinagulos 
rgbPalettePolygon :: Int -> [(Int,Int,Int)]
rgbPalettePolygon n = take n $ cycle [(0+r,0+r,r),(0+g,0+g,0+g),(0+b,b,0+b)]
      where r = 0
            g = 0
            b = 0
-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 10),w,h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (30,1000)
        gap = 0
-------------------------------------------------------------------------------
-- Geração de Circulos em suas posições
-------------------------------------------------------------------------------

genCircleInLine :: Int -> [Circle]
genCircleInLine n  = [((o*(gap1+2*r) + r*2, gap1+50*o),gap1+r*o) | o <- [1..fromIntegral (n)]]
  where r = 10
        gap1 = 20

-------------------------------------------------------------------------------
-- Geração de Ellipses em suas posições
-------------------------------------------------------------------------------
genEllipseInLine :: Int -> [Ellipse]
genEllipseInLine n  = [((p*(gap2+5*rx) + rx*2, 750),rx/p,ry*p+2) | p <- [1..fromIntegral (n)]]
  where rx = 20
        ry = 30
        gap2 = 5
-------------------------------------------------------------------------------
-- Geração de Linhas em suas posições
-------------------------------------------------------------------------------
genLineInLine :: Int -> [Line]
genLineInLine n  = [((q*20*gap3, 20),(x2, y2)) | q <- [0..fromIntegral (n-1)]]
  where x2 = 2500/2
        y2 = 1000/2
        gap3 = 70

-------------------------------------------------------------------------------
-- Geração de Poligonos em suas posições
-------------------------------------------------------------------------------
genPolygonInLine :: Int -> [Polygon]
genPolygonInLine n  = [((r*gap4+1250, x),(r*gap4+1300,y),(r*gap4+1200,z))| r <- [0..fromIntegral (n-1)]]
  where gap4 = 100
        x = 450
        y = 550
        z = 550
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

svgPolygon :: Polygon -> String -> String
svgPolygon ((x1,y1),(x2,y2),(x3,y3)) style =
  printf "<polygon points='%.3f,%.3f %.3f,%.3f %.3f,%.3f' fill ='%s' />\n" x1 y1 x2 y2 x3 y3 style
 
-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"
-------------------------------------------------------------------------------
--Gera string com atributos de estilo para uma dada cor
-------------------------------------------------------------------------------
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

-- Cor do Poligono
svgPolygonStyle :: (Int,Int,Int) -> String
svgPolygonStyle (r,g,b) = printf "rgb(%d,%d,%d)" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo

svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles


-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "main.svg" $ svgstrs --RETANGULO    LINHA        CIRCULO       ELIPSE        POLIGONO        FIM 
  where svgstrs = svgBegin w h  ++ svgfigsX ++ svgLineX ++ svgCircleX ++ svgEllipseX ++ svgPolygonX  ++ svgEnd
                 --INICIO 
        -----------------------------------------------------------------------
        svgfigsX = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInLine nrects
        palette = rgbPaletteRect nrects
        nrects = 100
        -----------------------------------------------------------------------
        svgCircleX = svgElements svgCircle circle (map svgCircleStyle pallete1)
        circle = genCircleInLine nCircle 
        pallete1 = rgbPaletteCircle nCircle
        nCircle = 10
        -----------------------------------------------------------------------
        svgEllipseX= svgElements svgEllipse ellipse (map svgEllipseStyle pallete2)
        ellipse = genEllipseInLine nEllipse 
        pallete2 = rgbPaletteEllipse nEllipse
        nEllipse = 10
        -----------------------------------------------------------------------
        svgLineX= svgElements svgLine line (map svgLineStyle pallete3)
        line = genLineInLine nLine 
        pallete3 = rgbPaletteLine nLine
        nLine = 10
        -----------------------------------------------------------------------
        svgPolygonX= svgElements svgPolygon polygon (map svgPolygonStyle pallete4)
        polygon = genPolygonInLine nPolygon 
        pallete4 = rgbPalettePolygon nPolygon
        nPolygon = 1
        -----------------------------------------------------------------------
        (w,h) = (2500,1000) -- Largura e Altura da imagem SVG (minha tela de desenho)
       