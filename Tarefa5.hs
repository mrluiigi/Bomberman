{- | 
Module : Tarefa 5
Description : Tarefa 5
Copyright : José Pinto,Luís Correia;
-}
module Main where

import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import Bomberman
import Data.List
import Data.Char
import Data.Maybe
import System.Random
import Tarefa6_li1g034
{- |O type estado é uma representação do estado de jogo. Este é constituído por uma lista de String, que é o mapa mais a informação dos Power Ups, Bombas e Jogadores, por uma lista de Picture, que são os bitmaps importados para representar os elementos do jogo
e por um Float que representa o contador do tempo.
-}
type Estado = ([String],[Picture],Float)
{- | A função estado inicial, recebe uma lista de picture que são os bitmaps, devolvendo o Estado.
-}
estadoInicial :: [Picture] -> Estado
estadoInicial p = (["Menu"], p, 0)
-- Função que desenha o jogo.
{-| A função desenhaEstado recebe o Estado e devolve uma picture, resultante de ter transformado cada caracter no seu bitmap, que é o desenho do jogo.  
-}
desenhaEstado :: Estado -> Picture
desenhaEstado (s,[p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19],f) | s == ["Menu"] = (Translate 0 0 $ Scale 0.7 0.48 p11)
                                                                                         | s == ["Player0"] = (Scale 0.7 0.7 p15)
                                                                                         | s == ["Player1"] = (Scale 0.7 0.7 p16)
                                                                                         | s == ["Player2"] = (Scale 0.7 0.7 p17)
                                                                                         | s == ["Player3"] = (Scale 0.7 0.7 p18)
                                                                                         | otherwise = desenhaEstado2 (s,[p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19],f) 

  where   desenhaEstado2 :: Estado -> Picture
          desenhaEstado2 (s,[p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19],f) = Pictures $ drawMapa (putOnMap (onlyMapa s) ((onlyPlayers s) ++ (onlyPu s) ++ ((onlyBombs s) \\ (whichBomb (onlyBombs s))) ))  (-200,125)

          drawMapa [] (x,y) = [(Scale 0.7 0.7 p8)] 
          drawMapa [h] (x,y) = drawSt h (x,y)
          drawMapa (h:t) (x,y) = (drawSt h (x,y)) ++ (drawMapa t (x,y-40)) 
          drawSt [h] (x,y) = [Translate x y $ Scale 0.19 0.19 p1] 
          drawSt (h:t) (x,y) | h == '#' = (Translate x y $ Scale 0.188 0.188 p1) : drawSt t (x+40,y)
                             | h == '?' = (Translate x y $ Scale 0.188 0.188 p2) : drawSt t (x+40,y)
                             | h == '*' = (Translate x y $ Scale 0.18 0.18 p7) : drawSt t (x+40,y)
                             | h == '0' = (Translate x y $ Scale 0.178 0.178 p4) : drawSt t (x+40,y)
                             | h == '1' = (Translate x y $ Scale 0.178 0.178 p3) : drawSt t (x+40,y)
                             | h == '2' = (Translate x y $ Scale 0.178 0.178 p9) : drawSt t (x+40,y)
                             | h == '3' = (Translate x y $ Scale 0.178 0.18 p10) : drawSt t (x+40,y)
                             | h == '+' = (Translate x y $ Scale 0.11 0.11 p6) : drawSt t (x+40,y)
                             | h == 'Z' = (Translate x y $ Scale 0.188 0.188 p5) : drawSt t (x+40,y)
                             | h == '!' = (Translate x y $ Scale 0.18 0.18 p19) : drawSt t (x+40,y)
                             | h == 'Y' = (Translate x y $ Scale 0.188 0.188 p12) : drawSt t (x+40,y)
                             | h == 'B' = (Translate x y $ Scale 0.18 0.18 p13) : drawSt t (x+40,y)
                             | h == 'U' = (Translate x y $ Scale 0.18 0.18 p14) : drawSt t (x+40,y)
                             | h == 'R' = (Translate x y $ Rotate 90 $ Scale 0.18 0.18 p14) : drawSt t (x+40,y)
                             | h == 'D' = (Translate x y $ Rotate 180 $ Scale 0.18 0.18 p14) : drawSt t (x+40,y)
                             | h == 'L' = (Translate x y $ Rotate 270 $ Scale 0.18 0.18 p14) : drawSt t (x+40,y)
                             | otherwise = (Translate x y $ Color white (rectangleSolid 40 40)) : drawSt t (x+40,y)
{-| A função reageEvento recebe a semente do gerador pseudo-aleatório, um Event e o Estado, devolvendo um novo Estado resultante do comando que os jogadores tenham executado. No início do jogo, o jogador pode executar o comando 
p, m ou g para escolher a dimensão do mapa que querem. 
-}
-- Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Int -> Event -> Estado -> Estado
reageEvento seed (EventKey (Char 'p') Down _ _) (s,p,f) = (makeMapa 7 seed,p,0)
reageEvento seed (EventKey (Char 'm') Down _ _) (s,p,f) = (makeMapa 9 seed,p,0)
reageEvento seed (EventKey (Char 'g') Down _ _) (s,p,f) = ((makeMapa 11 seed),p,0)
reageEvento _ (EventKey (SpecialKey KeyUp) Down _ _) (s,p,f) = (move1 s 1 'U',p,f)
reageEvento _ (EventKey (SpecialKey KeyDown) Down _ _) (s,p,f) = (move1 s 1 'D',p,f)
reageEvento _ (EventKey (SpecialKey KeyLeft) Down _ _)  (s,p,f) = (move1 s 1 'L',p,f)
reageEvento _ (EventKey (SpecialKey KeyRight) Down _ _) (s,p,f) = (move1 s 1 'R',p,f)
reageEvento _ (EventKey (SpecialKey KeyEnter) Down _ _) (s,p,f) = (move1 s 1 'B',p,f)
reageEvento _ (EventKey (Char 'w') Down _ _) (s,p,f) = (move1 s 0 'U',p,f)
reageEvento _ (EventKey (Char 's') Down _ _) (s,p,f) = (move1 s 0 'D',p,f)
reageEvento _ (EventKey (Char 'a') Down _ _) (s,p,f) = (move1 s 0 'L',p,f)
reageEvento _ (EventKey (Char 'd') Down _ _) (s,p,f) = (move1 s 0 'R',p,f)
reageEvento _ (EventKey (Char 'e') Down _ _) (s,p,f) = (move1 s 0 'B',p,f)
reageEvento _ _  (s,p,f) = (s,p,f)
{-| A função makeMapa recebe como input a dimensão do mapa e a semente, devolvendo a função mapa com os jogadores.
-}
makeMapa :: Int -> Int -> [String]
makeMapa d seed = (mapa1 d seed) ++ ["0 1 1","1 "++show(d-2)++" " ++ show(d-2),"2 1 " ++ show(d-2), "3 " ++ show (d-2) ++ " 1"]
{-| A função reageTempo, recebe um float que é o tempo do jogo e o Estado, devolvendo um novo Estado resultante de ter passado um instante de tempo.
-}
-- Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo f (s,p,t) | s == ["Menu"] = (s,p,t)
                     | length (onlyPlayers s) == 0 = (["D"],p,t)             
                     | length  (onlyPlayers s) == 1 = (chooseWinner (onlyPlayers s),p,t)
                     | otherwise = if time t then (avanca1 (move1 bot2 3 (fromMaybe 'Z'(bot s 3 (150 -(round t))))) (150 -(round t)),p,(f+t)) else  ((putOnMap (flamesOnMap (onlyMapa s) (whichBomb (onlyBombs s))) (onlyPu s)) ++ (s \\ onlyMapa s) ,p,(f+t))
                          
         where  bot2 = (move1 s 2 (fromMaybe 'Z' (bot s 2 (150 -(round t)))))

                chooseWinner :: [String] -> [String]
                chooseWinner s | head (head s) == '0' = ["Player0"]
                               | head (head s) == '1' = ["Player1"]
                               | head (head s) == '2' = ["Player2"]
                               | head (head s) == '3' = ["Player3"]
                               | otherwise = s 

                inteiro :: Float -> Bool
                inteiro f = if floor f == ceiling f then True else False

                flamesOnMap :: Mapa -> [String] -> Mapa -- marca os sitios que a bomba atinge com as chamas
                flamesOnMap [] _ = []
                flamesOnMap l [] = l
                flamesOnMap m (h:t) = flamesOnMap (putAux h m (posBomb h)) t
                              
                putAux :: String -> Mapa -> Position -> Mapa
                putAux _ [] _ = []
                putAux (h:t) m (x,y) = reverse (aux2 (reverse (take y m)) x (numFlame (h:t)) 'U') ++ [((reverse(aux h (reverse(take x (m!!y))) (numFlame (h:t)) 'L')) ++ "B" ++ (aux h (drop (x+1) (m!!y)) (numFlame (h:t)) 'R'))] ++ (aux2 (drop (y+1) m) x (numFlame (h:t)) 'D')
                                      

                aux :: Char -> String -> Int -> Char -> String -- Horizontal
                aux _ [] _ _ = []
                aux h (x:xs) n c | n == 1 = if x == '#' then (x:xs) else 
                                                                        if c == 'L' then ('L':xs) else ('R':xs)
                                 | x == '#' = (x:xs)
                                 | x == '?' = (if c == 'L' then ('L':xs) else ('R':xs))
                                 | otherwise = 'Y' : aux h xs (n-1) c

                aux2 :: [String] -> Int -> Int -> Char -> Mapa -- vertical
                aux2 [] _ _ _ = []
                aux2 (h:t) x n c | n == 1 = if (h!!x) == '#' then (h:t) else aux4 h x c : t
                                 | (h!!x) == '#' = (h:t)
                                 | (h!!x) == '?' = ((aux3 h x):t)
                                 | otherwise = aux3 h x : aux2 t x (n-1) c
  
                aux3 :: String -> Int -> String -- vertical
                aux3 h x = take x h ++ "Z" ++ drop (x+1) h 

                aux4 h x c | c == 'U' = take x h ++ "U" ++ drop (x+1) h 
                           | c == 'D' = take x h ++ "D" ++ drop (x+1) h          

                time :: Float -> Bool
                time f = even (round (f *100))
{- |O valor fr é o numero de simulações que se fazem por cada segundo.
-}
--  Frame rate
fr :: Int
fr = 4
{-|Descreve como o Gloss deve apresentar o output
-}
-- Display mode
dm :: Display
dm = InWindow "Bitmaps/Bomberman" (1366, 768) (-10, -10)
{-|O programa main é usado para correr o jogo.
-}
-- Função principal que invoca o jogo.
main ::  IO ()
main = do   p1 <- loadBMP "Bitmaps/Stone.bmp" 
            p2 <- loadBMP "Bitmaps/Brick.bmp" 
            p3 <- loadBMP "Bitmaps/DaffyDuck.bmp"
            p4 <- loadBMP "Bitmaps/BugsBunny.bmp"
            p5 <- loadBMP "Bitmaps/PuFlames.bmp"
            p6 <- loadBMP "Bitmaps/pubombs.bmp"
            p7 <- loadBMP "Bitmaps/dinamite.bmp"
            p8 <- loadBMP "Bitmaps/mudar.bmp"
            p9 <- loadBMP "Bitmaps/Taz.bmp"
            p10 <- loadBMP "Bitmaps/Tweety.bmp"
            p11 <- loadBMP "Bitmaps/BombermanMenu.bmp"
            p12 <- loadBMP "Bitmaps/ChamasH.bmp"
            p13 <- loadBMP "Bitmaps/ChamasC.bmp"
            p14 <- loadBMP "Bitmaps/ChamasP.bmp"
            p15 <- loadBMP "Bitmaps/Winner0.bmp" 
            p16 <- loadBMP "Bitmaps/Winner1.bmp"
            p17 <- loadBMP "Bitmaps/Winner2.bmp"
            p18 <- loadBMP "Bitmaps/Winner3.bmp"
            p19 <- loadBMP "Bitmaps/PuFlames.bmp"
            x <- randomIO     
            return (abs x)
            play dm              -- display mode
                 (greyN 0.5)     -- côr do fundo da janela
                 fr              -- frame rate
                 (estadoInicial  [p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19])-- estado inicial
                 desenhaEstado   -- desenha o estado do jogo
                 (reageEvento x)     -- reage a um evento
                 reageTempo      -- reage ao passar do tempo