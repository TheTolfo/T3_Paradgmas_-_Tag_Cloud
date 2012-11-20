{--
        Esqueleto de programa para geração de bubble cloud em Haskell.
        Mais informações em: http://www.inf.ufsm.br/~andrea/elc117-2012b
--}


-- Link para o repositório em: https://github.com/TheTolfo/T3_Paradgmas_-_Tag_Cloud.git

module Main where

import System.IO.Unsafe -- Para remover o IO do tipo IO Int ods valores gerados randomicamente
import System.Random -- Para gerar valores random (cor do circulo)
import Text.Printf -- Oba, Haskell tem printf! :-)
import Data.List -- Funções para lista

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Int
imageWidth = 360

imageHeight :: Int
imageHeight = 360


-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"
--
--
-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss
--
--
-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"
--
--
-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [svgCircle ((fromIntegral w/2, fromIntegral h/2), 10.0)]
--
--
-- Gera string representando um circulo em SVG. randomizando 3 valores para usar como RGB.
geraRand :: IO Int
geraRand = getStdRandom (randomR (0,255::Int))
--     
svgCircle :: Circle -> String
svgCircle ((x,y),r) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y r red green blue
  where
       red = unsafePerformIO geraRand
       green = unsafePerformIO geraRand
       blue = unsafePerformIO geraRand
--
--
-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h