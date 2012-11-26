-- Link para o repositório do projeto em: https://github.com/TheTolfo/T3_Paradgmas_-_Tag_Cloud.git

{--

OBS: A lib random não queria funcionar, então apenas a inclui como uma extençção no .cabal, se der algum erro na execução ou interpretação, 
 baixe a versão completa da lib no site http://www.haskell.org/ghc/
 

--}
module Main where
--
import System.IO.Unsafe -- Para remover o IO do tipo IO Int ods valores gerados randomicamente
import System.Random -- Para gerar valores random (cor do circulo)
import Text.Printf -- Oba, Haskell tem printf! :-)
import Data.List -- Funções para lista
--
--
-- Define os tipos de dados a ser utilizados
type Point = (Float,Float)
type Color = (Int,Int,Int)
type Circle = (Point,Float)
--
--
imageWidth :: Int
imageWidth = 360
--
imageHeight :: Int
imageHeight = 360 
--
--
-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent) 
            freqs = readInts (map snd pairs) -- le os valores inteiros do arquivo de entrada e os transforma em uma lista
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs) -- função que escreve no arquivo de saida
        putStrLn "Codigo SVG criado com sucesso!" -- mostra o termino da execução
        where 
             infile = "dataset.txt" -- defien o arquivo que será lido
             outfile = "tagcloud.svg" -- define o arquivo de saida
--
--
-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss  -- lê as taxas de frequência como inteiros
--
--
-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++  -- adiciona o cabeçalho do codigo svg
        (svgViewBox w h) ++ -- gera um quadrado com os tamanhos definidos anteriormente, com o fundo branco
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"  -- gera os circulos e termina a utilização do quadrado criado anteriormente.
--
--
-- Transforma as taxas de frequência em uma raios
transformaEmRaio :: [Int] -> [Float]
transformaEmRaio [] = []
transformaEmRaio dataset = r : transformaEmRaio (tail dataset)
  where
       elem = (head dataset)  -- pega o primeiros elemento da lista
       pr = fromIntegral elem/45 -- divide-o por 90 e o transforma é float
       r =  pr + 5 -- soma 5 (para os circulos possuirem raios de um tamanho visivel, por menor que seja a frequencia)
--
--
-- Funcão que adapta os dados e chama a criação dos circulos
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [geraTag (fromIntegral w/2) (fromIntegral h/2) (reverse (sort datR))] -- chama a função que gera os circulos em código svg
  where 
       datR = transformaEmRaio dataset -- transforma a lista de frequencia em uma lista de raios
--
--
-- Verifica distancia entre 2 pontos   d² = (x2 - x1)² + (y2 - y1)²
veDist :: Point -> Point -> Float
veDist (x1,y1) (x2,y2) = sqrt (xT + yT)
  where
       xT = (x2 - x1) ^ 2
       yT = (y2 - y1) ^ 2
--
--
-- Verifica se os circulos não estão sobrepotos, se a distancia entre os pontos for maior que a soma dos raios, retorna verdadeiro, se 
-- não estão, retorna falso
verificaPonto :: Circle -> Circle -> Bool
verificaPonto ((x1,y1),r1) ((x2,y2),r2) = if (dist >= 0.1)
  then True
  else False 
  where
       dist = (veDist (x1,y1) (x2,y2)) - r1 - r2 
--
--
-- Recursão que retorna uma lista do tipo bool, para testar se o ponto é valido ou não.
verificaP :: [Circle] -> Circle -> [Bool]
verificaP [] _ = []
verificaP listcirc circulo = test : (verificaP (tail listcirc) circulo)
  where 
       test = verificaPonto (head listcirc) circulo
--       
--
-- Gera um novo ponto valido (dentro da linha espiral e não sobrepondo outros circulos).
geraPonto :: [Circle] -> Float -> Float -> Float -> Point -> [Circle]
geraPonto circ t a nR centro = if (test == True)
  then [((nX, nY),nR)]
  else if(nR < 15)
          then geraPonto circ (t + 0.1) a nR centro
          else geraPonto circ (t + 5) a nR centro
  where
       nX = (fst centro) + (a * t * (cos t)) -- gera o novo x  
       nY = (snd centro) + (a * t * (sin t)) -- gera o novo y
       test = (and (verificaP circ ((nX,nY), nR))) -- verifica se o ponto gerado para o circulo é valido
--
--
-- Gera uma lista do tipo Circle com todos os dados necessários.
geraLista :: [Circle] -> Float -> Float -> [Float] -> Point-> [Circle]
geraLista circ a t datR centro = if (datR == [])
  then circ
  else (geraLista (circ ++ newPonto) a 0 (tail datR) centro)
  where newPonto = geraPonto circ t a (head datR) centro
--
--
-- Recursão que gera o código svg para os circulos
geraCod :: [Circle] -> String
geraCod [] = []
geraCod lista = svgCircle (head lista) ++ geraCod (tail lista) -- gera o código de um circulo e chama uma recursão apra gerar o dos próximos.
--
--
-- Gera uma string contendo todos os circulos e seus respectivos pontos.
geraTag :: Float -> Float -> [Float] -> String
geraTag _ _ [] = []
geraTag x y datR = geraCod mountedCircles --map (svgCircle ++) mountedCircles
  where
       mountedCircles = geraLista [((x,y),(head datR))] a 0 (tail datR) (x,y)
       a = (head datR + head (tail datR)) * 0.004077-- define um valor para o a
--
--
-- Gera um numero aleatório entre 0 e 255
geraRand :: IO Int
geraRand = getStdRandom (randomR (0,255::Int))
--    
-- Gera string representando um circulo em SVG. randomizando 3 valores para usar como RGB
svgCircle :: Circle -> String
svgCircle ((x,y),r) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y r red green blue-- gera o codigo svg de cada circulo com uma cor randomica
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