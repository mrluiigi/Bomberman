{- | 
Module : Tarefa 3
Description : Tarefa3
Copyright : José Pinto,Luís Correia;
-}
module Tarefa6_li1g034 where
  
import Bomberman
import Data.List
{- | A função change altera o type Vizinhança para uma lista

=Exemplo
>>> change (' ','#','?','*',' ')
" #?* "
-}
change :: Vizinhança -> String
change (a,b,c,d,e) = [a,b,c,d,e]
{- | A função safeSpot recebe o mapa proveniente da função dangerOnMap, com os locais de perigo assinalados no mapa, recebe a Vizinhança de uma posição e essa uma posição e recebe também dois contadores, o primeiro para que a função safeSpot tenha um término e o segundo para saber a distancia da posição dada até o local seguro,
devolvendo uma lista de tuplos, cada um com 3 elementos, o primeiro é a distância até o local seguro, o segundo a posição do local seguro e o terceiro o Maybe Char que o bot deverá fazer para chegar até o local seguro.

=Exemplo
>>> safeSpot (["#####","#*ZZ#","#Z# #","#Z  #","#####","* 1 1 0 4 10"]) ('*','Z','#','#','Z') (1,2) 10 0
[(1234512345,(0,0),Nothing),(8,(2,3),Just 'U'),(8,(3,2),Just 'U'),(6,(2,3),Just 'U'),(6,(3,2),Just 'U'),(4,(2,3),Just 'U'),(4,(3,2),Just 'U'),(2,(2,3),Just 'D')]
>>> safeSpot (["#####","#*ZZ#","#Z#Z#","#ZZ*#","#####","* 1 1 0 4 10"]) ('*','Z','#','#','Z') (1,2) 10 0
[(1234512345,(0,0),Nothing)]
-}
safeSpot :: [String] -> Vizinhança -> Position -> Int -> Int -> [(Int, Position, Maybe Char)]
safeSpot _ _ _ 0 _ = [(1234512345,(0,0), Nothing)] 
safeSpot m (a,b,c,d,e) (x,y) n q | a == ' ' = [(q+1,(x,y-1), Just 'U')]
                                 | b == ' ' = [(q+1,(x,y+1), Just 'D')]
                                 | c == ' ' = [(q+1,(x-1,y) ,Just 'L')]
                                 | d == ' ' = [(q+1,(x+1,y), Just 'R')]
                                 | otherwise = nub(aux2 m (aux m (change (a,b,c,d,e)) [(q+1,(x,y-1),Just 'U'),(q+1,(x,y+1),Just 'D'),(q+1,(x-1,y),Just 'L'),(q+1,(x+1,y),Just 'R')]) n)
    where aux :: [String] -> String -> [(Int,Position, Maybe Char)] -> [(Int,Position,Maybe Char)]
          aux m [] l = l
          aux m l [] = []
          aux m (a:r) ((h,hs,c):t) | a == '#' || a == '?' = aux m r t 
                                   | otherwise = (h,hs,c) : aux m r t

          aux2 :: [String] -> [(Int,Position,Maybe Char)] -> Int -> [(Int,Position,Maybe Char)]
          aux2 m [] _ = []
          aux2 _ _ 0 = []
          aux2 m ((c,h,hs):t) n = (join(safeSpot m (near(takeL m h) h) h (n-1) c) hs) ++ aux2 m t (n-1) 

          join :: [(Int,Position,Maybe Char)] -> Maybe Char -> [(Int,Position,Maybe Char)] -- para que fique com a primeiro char
          join [] _ = []
          join ((i,p,c):t) c2 | c == Nothing = (i,p,c) : join t c2
                              | otherwise= (i,p,c2) : join t c2
{- | A função chooseSafeSpot recebe a lista de tuplos provinda da função safeSpot e devolve o Maybe Char daquele que tenha uma distância menor (a que tem o primeiro elemento menor).

=Exemplo
>>> chooseSafeSpot [(1234512345,(0,0),Nothing),(8,(2,3),Just 'U'),(8,(3,2),Just 'U'),(6,(2,3),Just 'U'),(6,(3,2),Just 'U'),(4,(2,3),Just 'U'),(4,(3,2),Just 'U'),(2,(2,3),Just 'D')]
Just 'D'
>>> chooseSafeSpot [(1234512345,(0,0),Nothing)]
Nothing
-}
chooseSafeSpot :: [(Int, Position, Maybe Char)] -> Maybe Char
chooseSafeSpot [] = Nothing
chooseSafeSpot l = trd (minimum l)
        where trd :: (Int,Position,Maybe Char) -> Maybe Char
              trd (i,p,c) = c
{- | A função letsBomb recebe o mapa, a vizinhança e a posição e o numero do bot de forma a verificar se caso o bot coloque uma bomba, este consegue fugir ou não, devolvendo o Bool correspondente. 

=Exemplo
>>> letsBomb (["#####","#   #","# # #","#   #","#####","0 1 1"]) ('#',' ','#',' ',' ') (1,1) 1
True
>>> letsBomb letsBomb (["#####","#*ZZ#","#Z#Z#","#ZZ*#","#####","0 1 3","* 1 1 0 4 10"]) ('Z','#','#','Z','Z') (1,3) 0
False 
-}
letsBomb :: [String] -> Vizinhança -> Position -> Int -> Bool 
letsBomb m v (x,y) p  | (chooseSafeSpot(safeSpot (dangerOnMap (onlyMapa m1) (onlyBombs m1)) v (x,y) 10 0)) == Nothing = False
                      | otherwise = True
                          where m1 = m ++ ["* " ++ show (x) ++ " " ++ show (y) ++ show (p) ++ " " ++ show (temFlames m p) ++ " 1"]
{- | A função meioVai recebe o mapa proveniente da função dangerOnMap, com os locais de perigo assinalados no mapa, a vizinhança do bot, a posição atual do bot e umas coordenadas proximas do centro do mapa e devolve um Maybe Char correspondeente ao comando mais apropiado para o levar para o centro do mapa  

=Exemplo
>>> meioVai ["#####","#   #","# # #","#   #","#####","0 1 1"] ('#',' ','#',' ',' ') (1,1) (1,2)
Just 'D'
-}
meioVai :: [String] -> Vizinhança -> Position -> Position -> Maybe Char
meioVai m (a,b,c,d,e) (x,y) (mx, my) | (x < mx) && (d == ' ') = Just 'R'
                                     | (y < my) && (b == ' ') = Just 'D'
                                     | (y > my) && (a == ' ') = Just 'U'
                                     | (x > mx) && (c == ' ') = Just 'L'
                                     | (x == mx) && ( y == my) = Nothing
                                     | d == ' ' = Just 'R'
                                     | b == ' ' = Just 'D'
                                     | a == ' ' = Just 'U'
                                     | c == ' ' = Just 'L'
                                     | otherwise = Nothing 
{- | A função caracolDanger é quase identica à caracol, so que em vez de colocar um '#' no sitio adequado, coloca um 'Z' para que o bot evite essa zona.

=Exemplo
>>> putStr(unlines(caracolDanger ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######"] 0))
#######
#Z    #
# # # #
# ??  #
# #?# #
#     #
#######
>>> putStr(unlines(caracolDanger ["#######","#######","# # ###","# ?? ##","# #?###","#   ###","#######"] 0))
#######
#######
# # ###
# ?? ##
# #?###
#  Z###
#######
-}
caracolDanger :: Mapa -> Int -> Mapa -- Int contador == 0 
caracolDanger [] _ = []
caracolDanger l n | (filter (not .null ) $ map ( filter (/='#')) l) == [] = l 
                  | (isfull $ l !! n) == False = (take n l) ++ [(aux $ l !! n)] ++ (drop (n+1) l) 
                  | (isLast l n) == False = putLast l n
                  | (isfull $ (reverse l) !! n) == False = (take (length l -n-1) l) ++ [reverse $ aux (reverse (l !! (length l - n-1)))] ++ (drop (length l - n) l) 
                  | (isFirst l n) == False = reverse(putFirst (reverse l) n)
                  | otherwise = caracolDanger l (n+1)
  where isfull [] = True -- vê se uma string é toda de #
        isfull (h:t) = (h == '#') && isfull t

        aux [] = [] -- poe um # na 2 string 
        aux (h:t) = if h == '#' then h: aux t else 'Z' :t 
     
        putLast [] _ = [] -- mete numa lista de strings um #
        putLast (h:t) n = if ((reverse h) !! n) == '#' then h:putLast t n else reverse (aux (reverse h)) : t

        isLast [] _ = True  -- vê se uma [String] tem o penúltimo char preenchido
        isLast (h:t) n = if ((reverse h) !! n) == '#' then isLast t n else False

        isFirst [] _ = True
        isFirst (h:t) n = if (h !! n) == '#' then isFirst t n else False

        putFirst [] _ = []
        putFirst (h:t) n = if (h !! n) == '#' then h : putFirst t n else (aux h) : t
{- | A função nearPlayerBrick recebe o mapa e a posição do bot e determina se existem jogadores ou tijolos em seu redor.

=Exemplo
>>> nearPlayerBrick ["#####","#   #","# # #","#   #","#####","0 1 1"]  (1,1) 
False
>>> nearPlayerBrick ["#####","# ? #","# # #","#   #","#####","0 1 1"]  (1,1) 
True
-}
nearPlayerBrick :: [String]-> Position ->  Bool
nearPlayerBrick m p =  or $ zipWith elem "0123?" [vizinhança,vizinhança,vizinhança,vizinhança,vizinhança]
                          where vizinhança =(init $ change (near ( takeL ( putOnMap (onlyMapa m) (onlyPlayers m)) p) p))                           
{- | A função mexe recebe o mapa proveniente de dangerOnMap  e a vizinhaça, a posição e o numero do bot, verifica qual a melhor ação e devolve o Maybe Char correspondente. 

=Exemplo
>>> mexe ["#####","#   #","# # #","#   #","#####","0 1 1","* 1 2 0 1 10"] ('#',' ','#',' ',' ')  (1,1) 0
Just 'D'
-}
mexe :: [String] -> Vizinhança -> Position ->  Int -> Maybe Char
mexe m (a,b,c,d,e) (x,y) p | e == 'Z' || e =='*' = (chooseSafeSpot(safeSpot (dangerOnMap (onlyMapa m) (onlyBombs m))(a,b,c,d,e) (x,y) 10 0))
                           | a == '+' || a == '!' = Just 'U'
                           | b == '+' || b == '!' = Just 'D'
                           | c == '+' || c == '!' = Just 'L'
                           | d == '+' || d == '!' = Just 'R'
                           | (nearPlayerBrick m (x,y)) && (meteuBomb m p < ((temBombs m p)+1)) && (letsBomb m (a,b,c,d,e) (x,y) p )== True = Just 'B'  
                           | otherwise = meioVai m (a,b,c,d,e) (x,y) (pos - 2 , pos - 1 )
                                                                      where pos = div (length (head m) +1)  2                                  
{- | A função bot recebe o estado atual do jogo, o identificador do jogador correspondente ao bot e o número instantes de tempo que faltam para o jogo terminar, devolvendo
o comando a executar ou Nothing se não quiser efectuar nenhum comando.

=Exemplo
>>> bot ["#####","#   #","# # #","#   #","#####","0 1 1","* 1 2 0 1 10"] 0 20
Just 'R'
>>> bot ["#####","##  #","# # #","#   #","#####","0 2 2","* 1 2 0 1 10"] 0 49
Just 'R'
>>> bot ["#########","#  ??   #","# # # # #","#??? ?  #","# #?#?#?#","# ?   ??#","# #?#?# #","#       #","#########","0 3 4"] 0 40
Just 'B'
-}
bot :: [String] -> Int -> Int -> Maybe Char  --estado do jogo é sem power ups
bot m p t | t <= ((length(head m)-2)^2 ) = mexe ((dangerOnMap (caracolDanger (caracol (onlyMapa m)0)0) (onlyBombs m))++(onlyPu m) ++ (onlyPlayers m)) (near(takeL (dangerOnMap (caracolDanger (caracol(onlyMapa m)0)0) (onlyBombs m)) h) h) h p
          | otherwise = mexe ((dangerOnMap (onlyMapa m) (onlyBombs m))++(onlyPu m) ++ (onlyPlayers m)) (near(takeL (dangerOnMap (onlyMapa m) (onlyBombs m)) h) h) h p
                 where h = pickPlayer m p