{- | 
Module : Bomberman
Description : Bomberman
Copyright : José Pinto,Luís Correia;
-}
module Bomberman where

import Data.Char (isDigit)
import Data.List
import System.Environment
import Text.Read
import System.Random
import Data.Maybe
import System.IO


{- |O type Mapa é uma representação do mapa do jogo, sem as Strings sobre os power ups,os jogadores e as bombas.
-}

type Mapa = [String]


--Tarefa 1

{- | A funçao putSpace cria uma linha generica com d elementos, um bloco de pedra (#) em cada extremo e no celulas indefinidas (%) no interior.

=Exemplo
>>> putSpace 9 
"#%%%%%%%#"
-}
putSpace :: Int -> String
putSpace d = "#" ++ (replicate (d-2) '%')  ++ "#"
{- | A funçao betSpace cria uma linha generica com d elementos, um bloco de pedra (#) em cada extremo e no interior alterna entre celulas indefinidas e blocos de pedra.

=Exemplo
>>>betSpace 7
"#%#%#%#""
-}
betSpace :: Int -> String  
betSpace 1 = "#"
betSpace n = "#" ++ "%" ++ (betSpace (n-2))
{- | A funçao secMapa gera a segunda linha do mapa, tendo em conta a necessidade de existirem espaços livres para possibilitar o movimento inicial dos jogadores.

=Exemplo
>>>secMapa 11 
"#  %%%%%  #"
-}
secMapa :: Int -> String
secMapa 5 = "#" ++ "   " ++ "#"
secMapa d = "#" ++ "  " ++ (replicate (d-6) '%') ++ "  " ++ "#"
{-| A funçao trdMapa gera a terceira linha do mapa, tendo em conta a necessidade de existir um espaço livre para possibilitar o movimento inicial dos jogadores.

=Exemplo
>>>trdMapa 11 
"# #%#%#%# #"
-}
trdMapa :: Int -> String
trdMapa 5 = "# # #"
trdMapa d = "#" ++ " " ++ (betSpace (d-4)) ++ " " ++ "#"
{-| A funçao repList recorre as funçoes putSpace e betSpace para gerar todas as linhas genericas do mapa.

=Exemplo
>>>repList 3 (putSpace 9) (betSpace 9)
["#%%%%%%%#","#%#%#%#%#","#%%%%%%%#"]
-}
repList :: Int -> String -> String -> [String]
repList 1 l s = [l]
repList x l s = l : s : (repList (x-2) l s)
{-| A funçao preMapa, com auxilio das funçoes definidas previamente, produz um pre-mapa (uma versao do mapa com apenas os blocos de pedra, os espaços livres em cada canto e as celulas indefinidas).

=Exemplo
>>>preMapa 9 
["#########","#  %%%  #","# #%#%# #","#%%%%%%%#","#%#%#%#%#","#%%%%%%%#","# #%#%# #","#  %%%  #","#########"]
-}
preMapa :: Int -> [String]
preMapa 5  = [replicate 5 '#'] ++ [secMapa 5] ++ [trdMapa 5] ++ [secMapa 5] ++ [replicate 5 '#']
preMapa d  = [replicate d '#'] ++ [secMapa d] ++ [trdMapa d] ++ (repList (d-6) (putSpace d) (betSpace d)) ++ [trdMapa d] ++ [secMapa d] ++ [replicate d '#']
{-| A funçao putGerador vai utilizar os valores de uma lista de inteiros aleatoria para substituir as celulas indefinidas por espaços vazios, por tijolos ou por power-ups. 

=Exemplo
>>>putGerador ["# %%%%% #"] [83,93,63,38,0,87,81,1,61] 
"#    ?+ #"
-}
putGerador :: [String] -> [Int] -> [String]
putGerador [] _  = []
putGerador (x:xs) g  = (aux x g ) : (putGerador xs (drop (aux2 x) g))                                
        where aux [] _  = []
              aux (h:t) (n1:n)  | h == '%' && (n1 == 0 || n1 == 1) = '+' : aux t n 
                                | h == '%' && (n1 == 2 || n1 == 3) = '!' : aux t n      
                                | h == '%' && (n1 < 40) = '?' : aux t n 
                                | h == '%' && (n1 >= 40) = ' ' : aux t n 
                                | h == '#' = '#' : aux t (n1:n) 
                                | otherwise = ' ' : aux t (n1:n)     
              aux2 [] = 0 
              aux2 (h:t) | h == '%' = 1 + aux2 t 
                         | otherwise = aux2 t  
{-| A funçao auxPuBomb recebe o mapa e devolve os Power Ups Bombs e as suas coordenadas.

=Exemplo
>>>auxPuBomb  ["#######","#  !   #","#+# # #","# ?   #","#!# #+#","#  +  #","#######"] (0,0)
["+ 1 2","+ 5 4","+ 3 5"]
-}
auxPuBomb :: [String] -> (Int,Int) -> [String]  
auxPuBomb [] _ = []
auxPuBomb (h:t) (x,y) = (auxPuBomb2 h (x,y)) ++ (auxPuBomb t (x,y+1)) 
{-| A funçao auxPuBomb2 recebe uma linha do mapa e devolve os Power Ups Bombs existentes e as suas coordenadas.

=Exemplo
>>>auxPuBomb2  "#!# #+#" (0,0)
["+ 5 0"]
-}
auxPuBomb2 :: String -> (Int,Int) -> [String]
auxPuBomb2 [] (x,y) = []
auxPuBomb2 (h:t) (x,y) | h == '#' = auxPuBomb2 t (x+1,y)
                       | h == '?' = auxPuBomb2 t (x+1,y)
                       | h == '+' = ["+ " ++ (show x) ++ " " ++ (show y)] ++ auxPuBomb2 t (x+1,y)
                       | h == '!' = auxPuBomb2 t (x+1,y)
                       | h == ' ' = auxPuBomb2 t (x+1,y)
{-| A funçao auxPuFlames recebe o mapa e devolve os Power Ups Flames e as suas coordenadas.

=Exemplo
>>>auxPuFlames ["#######","#  !   #","#+# # #","# ?   #","#!# #+#","#  +  #","#######"] (0,0)
["! 3 1","! 1 4"]
-}
auxPuFlames :: [String] -> (Int,Int) -> [String]  
auxPuFlames [] _ = []
auxPuFlames (h:t) (x,y) = (auxPuFlames2 h (x,y)) ++ (auxPuFlames t (x,y+1)) 
{-| A funçao auxPuFlames2 recebe uma linha do mapa e devolve os Power Ups Flames existentes e as suas coordenadas.

=Exemplo
>>>auxPuFlames2  "#!# #+#" (0,0)
["! 1 0"]
-}
auxPuFlames2 :: String -> (Int,Int) -> [String]
auxPuFlames2 [] (x,y) = []
auxPuFlames2 (h:t) (x,y) | h == '#' = auxPuFlames2 t (x+1,y)
                         | h == '?' = auxPuFlames2 t (x+1,y)
                         | h == '+' = auxPuFlames2 t (x+1,y)
                         | h == '!' = ["! " ++ (show x) ++ " " ++ (show y)] ++ auxPuFlames2 t (x+1,y)
                         | h == ' ' = auxPuFlames2 t (x+1,y)
{-| A funçao storePu recebe o mapa e , atraves das funçoes auxPuBom e auxPuFlames, retorna, de forma ordenada, todos os Power Ups e as suas coordenadas.

=Exemplo
>>>storePu  ["#######","#  !   #","#+# # #","# ?   #","#!# #+#","#  +  #","#######"] (0,0) 
["+ 1 2","+ 5 4","+ 3 5","! 3 1","! 1 4"]
-}
storePu :: [String] -> (Int,Int) -> [String]
storePu l (x,y) = (auxPuBomb l (x,y)) ++ (auxPuFlames l (x,y))
{-| A funçao ver recebe uma linha do mapa e substitui as representaçoes dos Power Ups (+ e !) por tijolos (?).

=Exemplo
>>>ver  "#!# #+#" 
"#?# #?#"
-}
ver :: String -> String
ver [] = []
ver (h:t) | h == '#' = '#' : ver t
          | h == '?' = '?' : ver t
          | h == '+' = '?' : ver t
          | h == '!' = '?' : ver t
          | otherwise = ' ' : ver t
{-| A funçao takeMapa recebe o mapa e utliza a funçao ver para substituir todas as representaçoes dos Power Ups (+ e !) por tijolos (?).

=Exemplo
>>>takeMapa  ["#######","#  !   #","#+# # #","# ?   #","#!# #+#","#  +  #","#######"] 
["#######","#  ?   #","#?# # #","# ?   #","#?# #?#","#  ?  #","#######"]
-}
takeMapa :: [String] -> [String]
takeMapa [] = []
takeMapa (h:t) = (ver h) : (takeMapa t)
{-| A funçao mapa, dada uma dimensao e uma semente, retorna um mapa com a dimensao dada e com uma listagem de todos os Power Ups presentes.

=Exemplo
>>>putStr ( unlines (mapa 9 0))
#########
#       #
# #?#?# #
#  ?  ? #
#?# # #?#
# ?  ?  #
# #?#?# #
#  ??   #
#########
+ 5 2
+ 3 3
! 5 5
-}
mapa1 :: Int -> Int -> [String]
mapa1 d s = takeMapa (putGerador (preMapa d) (take (d*d) (randomRs (0,99) (mkStdGen s))) ) ++ (storePu (putGerador (preMapa d) (take (d*d) (randomRs (0,99) (mkStdGen s)))) (0,0))


--Tarefa 2 


{- | O type Vizinhança é constituído por um tuplo de Char que são os caracteres que estão acima, abaixo, à esquerda, à direita de certa posição e também o caracter que está nessa posição. 
-}
type Vizinhança = (Char,Char,Char,Char,Char)
{- | O type Position é um par de Inteiros que representa a posição de um Jogador, de um Power Ups ou de uma Bomba.
-}
type Position = (Int,Int)
{- |A função pickPlayer recebe o mapa atual, um jogador e devolve a posição desse mesmo jogador no mapa.

=Exemplo 
>>> pickPlayer ["#####","#   #","# # #","#   #","#####","0 1 1"] 0 
(1,1)
>>> pickPlayer ["#####","#   #","# # #","#   #","#####","0 1 1","1 1 2"] 1 
(1,2)
-}
pickPlayer :: [String] -> Int -> (Int,Int)
pickPlayer [] _ = (0,0) 
pickPlayer (h:t) j | [(head h)] == (show j) = (read (words h !! 1), read (words h !! 2))
                   | otherwise = pickPlayer t j
{- |A função posPu recebe a linha do mapa onde se encontra o Power Up e o Power up que pretendemos e devolve a posição que esse Power Up tem no mapa.

=Exemplo
>>> posPu "+ 1 2" '+' 
(1,2)
>>> posPu "! 2 4" '!'
(2,4)
-}
posPu :: String -> Char -> (Int,Int)
posPu l p | (head l) == p = (read (words l !! 1), read (words l !! 2))
          | otherwise = (-1,-1)
{- |A função insere, uma função auxiliar da putBomb, recebe a linha do mapa onde se encontra a bomba e um inteiro que é a posição onde a bomba se encontra nessa linha e devolve a mesma linha só que com um '*' no local onde está a bomba.

=Exemplo 
>>> insere "# # #" 1 
"#*# #"
>>> insere "# #?# #" 5 
"# #?#*#"
-}
insere :: String -> Int -> String
insere (h:t) x | x /= 0 = h:insere t (x-1)
               | otherwise = '*' : t                       
{- |A função putBomb recebe o mapa atual com pelo menos uma bomba colocada, o jogador que colocou essa bomba, a posição em que a bomba foi posta e devolve o mapa com o caracter '*' na posição em que a bomba está colocada.

=Exemplo 
>>> putBomb ["#####","#   #","# # #","#   #","#####","* 2 3 1 1 10","0 1 1","1 2 3"] '1' (2,3) 
["#####","#   #","# # #","# * #","#####","* 2 3 1 1 10","0 1 1","1 2 3"]
-}
putBomb :: [String] -> Char -> (Int,Int) -> [String]
putBomb (h:t) j (x,y) | y /= 0 = h : putBomb t j (x,y-1)
                      | otherwise = (insere h x) : t
{- |A função takeL recebe o mapa atual, uma posição e devolve uma lista com 3 Strings (3 linhas do mapa): a linha correspondente à posição inserida, a linha que está antes e a que está depois.

=Exemplo 
>>> takeL ["#####","#   #","# # #","#   #","#####"] (3,2)
["#   #","# # #","#   #"]
>>> takeL ["#######","#     #","# # # #","#     #","# # # #","#     #","#######"] (1,5)
["# # # #","#     #","#######"]
-}
takeL :: [String] -> (Int,Int) -> [String]
takeL l (_,0) = take 2 l
takeL (h:t) (x,y) | y == 1 = h : takeL t (x,y-1)   
                  | otherwise = takeL t (x,y-1)
{- |A função pick recebe uma linha do mapa e a primeira coordenada da posição e devolve o caracter que está nessa posição.

=Exemplo 
>>> pick "#####"" 1
'#'
>>> pick "# # #" 1
' '
>>> pick "# #?# #" 3
'?'
-}
pick :: String -> Int -> Char
pick [] _ = ' '
pick (h:t) x | x == 0 = h
             | otherwise = pick t (x-1)
{- |A função near recebe uma lista de Strings (com 3 Strings) resultante da função takeL, uma posição (usando apenas a coordenada horizontal) e devolve a Vizinhança deste.

=Exemplo 
>>> near ["#   #","# # #","#   #"] (1,2)
(' ',' ','#','#',' ')
>>> near ["#     #","# # # #","# ?   #"] (2,3)
(' ','?',' ',' ','#') 
-}
near :: [String] -> Position -> Vizinhança
near (h:m:t) (x,y) = (a,b,c,d,e)
        where a = pick h x
              b = pick (head t) x
              c = pick m (x-1)
              d = pick m (x+1)
              e = pick m x
{- |A função canMove recebe a posição do jogador, o movimento que este pretende executar e a Vizinhança, devolvendo a posição resultante do movimento que este fez.

=Exemplo 
>>> canMove (1,2) 'U' (' ','?',' ',' ','#')
(1,1)
>>> canMove (1,2) 'D' (' ','?',' ',' ','#')
(1,2)
-}
canMove :: Position -> Char -> Vizinhança -> Position
canMove (x,y) p (a,b,c,d,e) | p == 'U' = if (a /= '#') && (a /= '?') && (a /= '*' ) then (x,y-1) else (x,y)
                            | p == 'D' = if (b /= '#') && (b /= '?') && (b /= '*' ) then (x,y+1) else (x,y)
                            | p == 'L' = if (c /= '#') && (c /= '?') && (c /= '*' ) then (x-1,y) else (x,y)
                            | p == 'R' = if (d /= '#') && (d /= '?') && (d /= '*' ) then (x+1,y) else (x,y)
                            | otherwise = (x,y)
{- |A função temFlames recebe o mapa atual, o jogador e devolve o nº de Flames que este tem.

=Exemplo 
>>> temFlames ["#####","#   #","# # #","#   #","#####","0 1 3"] 0
1
>>> temFlames ["#######","#     #","# # # #","# ?   #","# # # #","#     #","#######","+ 1 3","0 1 4 +","1 1 4 !"] 1
2
-}
temFlames :: [String] -> Int -> Int 
temFlames (h:t) j | [(head h)] == show j = 1 + (length $ filter (=='!') h)
                  | otherwise = temFlames t j
{- |A função temBombs recebe o mapa ayual, o jogador e devolve o nº de bombas que este tem acumuladas.

=Exemplo 
>>> temBombs ["#####","#   #","# # #","#   #","#####","0 1 3"] 0
0
>>> temBombs ["#######","#     #","# # # #","# ?   #","# # # #","#     #","#######","+ 1 3","0 1 4 +","1 1 4 !"] 0
1
-}              
temBombs :: [String] -> Int -> Int
temBombs [] _ = 0
temBombs (h:t) j | [(head h)] == show j = length $ filter (=='+') h
                 | otherwise = temBombs t j
{- |A função meteuBomb recebe o mapa atual, o jogador e devolve o nº de Bombas que este pôs

=Exemplo 
>>> meteuBomb ["#####","#   #","# # #","#   #","#####","0 1 3"] 0
0
>>> meteuBomb ["#####","#   #","# # #","#   #","#####","* 1 3 0 1 10","0 1 3"] 0
1
-}          
meteuBomb :: [String] -> Int -> Int
meteuBomb [] _ = 0
meteuBomb (h:t) j = length (filter (\(h:t)-> ((h == '*') && ([pick t 5] == (show j)))) (h:t)) 
{- |A função canBomb recebe a posição do jogador, o mapa atual, o jogador, o nº de Flames que tem e a Vizinhança, devolvendo se possivel a String resultante de o jogador ter colocado uma bomba, caso contrário, devolve a lista vazia.

=Exemplo 
>>> canBomb (1,2) ["#####","#   #","# # #","#   #","#####","0 1 2"] 0 1 (' ',' ','#','#',' ')
"* 1 2 0 1 10"
>>> canBomb (1,2) ["#####","#   #","# # #","#   #","#####","* 1 2 1 1 8","0 1 2","1 1 1"] 0 1 (' ',' ','#','#','*')
""
-}
canBomb :: Position -> [String] -> Int -> Int -> Vizinhança -> String
canBomb (x,y) m j r (a,b,c,d,e) = if ((e == '*') || (meteuBomb m j) >= (temBombs m j) +1) 
                                  then []
                                  else ("* " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show j) ++ " " ++ (show r) ++ " " ++ "10")
{- |A função changePos recebe o mapa atual, o jogador e a nova posição do jogador (seja esta igual à inicial ou não), devolvendo o mapa resultante das alterações feitas.

=Exemplo 
>>> changePos ["#####","#   #","# # #","#   #","#####","0 1 2"] 0 (1,1)
["#####","#   #","# # #","#   #","#####","0 1 1"]
>>> changePos ["#####","#   #","# # #","#   #","#####","0 1 2"] 0 (1,2)
["#####","#   #","# # #","#   #","#####","0 1 2"]
-}    
changePos :: [String] -> Int -> Position -> [String]
changePos [] _ _ = []
changePos ((x:xs):t) j (y,z) | [x] == (show j) = ((x : troca xs (y,z)) : t)
                             | otherwise = (x:xs) : changePos t j (y,z)
                    where troca (x1:xs) (x,y) | elem '+' xs || elem '!' xs = [x1] ++ (show x) ++ " " ++ (show y) ++ " " ++ filter (=='+')  xs ++ filter (=='!') xs
                                              | otherwise = [x1] ++ (show x) ++ " " ++ (show y)
{- |A função addPu recebe o mapa atual, um jogador x e o Power Up que apanhou (no caso de ter apanhado algum), devolvendo esse mesmo mapa, apena[x1] ++ (show x) ++ " " ++ (show y)s tendo o jogador x mais um Power Up (no caso de ter apanhado algum).

=Exemplo 
>>> addPu ["#####","#   #","# # #","#   #","#####","! 1 2","0 1 2 !"] 0 '!'
["#####","#   #","# # #","#   #","#####","! 1 2","0 1 2 !!"][x1] ++ (show x) ++ " " ++ (show y)
-}     
addPu :: [String] -> Int -> Char -> [String]
addPu [] _ _ = []
addPu ((x:xs):t) j p = if [x] == show j then if ((last xs =='+')||(last xs =='!')) then ordPu(((x:xs) ++ [p])) : t
                                             else ((x:xs) ++ " " ++ [p]) : t
                       else (x:xs) : addPu t j p 
{- |A função ordPu recebe a String que representa o jogador e ordena os seus Power Ups (primeiro os Bombs e depois os Flames) 

=Exemplo 
>>> addPu ["#####","#   #","# # #","#   #","#####","! 1 2","0 1 2 !"] 0 '!'
["#####","#   #","# # #","#   #","#####","! 1 2","0 1 2 !!"]
-}  
ordPu ::  String -> String
ordPu l = take 6 l ++ filter (== '+') l  ++ filter (== '!') l  
{- |A função pickPu recebe o mapa resultante da função addPu, o jogador e retira a linha do Power Up que foi apanhado por esse jogador

=Exemplo 
>>> pickPu ["#####","#   #","# # #","#   #","#####","! 1 2","0 1 2 !"] 0
["#####","#   #","# # #","#   #","#####","0 1 2 !!"]
-}  
pickPu :: [String] -> Int -> [String]
pickPu [] _ = []
pickPu [x] _ = [x]
pickPu (h:t) j | posPu h '+' == pickPlayer (h:t) j = addPu t j '+' 
               | posPu h '!' == pickPlayer (h:t) j = addPu t j '!'             
               | otherwise = h : pickPu t j
{- |A função onlyMapa recebe o mapa atual e com os Power Ups e devolve apenas o mapa sem as Bombas e sem os Jogadores.

=Exemplo 
>>> onlyMapa ["#####","#   #","# # #","#   #","#####","! 1 2","0 1 2 !"]
["#####","#   #","# # #","#   #","#####"]
-}  
onlyMapa :: [String] -> [String]
onlyMapa [] = []
onlyMapa l = filter (\(h:t) -> (h == '#')) l 
{- |A função onlyPu recebe o mapa atual e devolve apenas a(s) String(s) dos Power Ups.

=Exemplo 
>>> onlyPu ["#####","#   #","# # #","#   #","#####","! 1 2","0 1 2 !"]
["! 1 2"]
>>> onlyPu ["#####","#   #","# # #","#   #","#####","+ 1 3","! 1 2","* 1 2 0 1 10","0 1 2 !"]
["+ 1 3","! 1 2"]
-} 
onlyPu :: [String] -> [String]
onlyPu [] = []
onlyPu l = filter (\(h:t) -> (h == '+' || h == '!')) l
{- |A função onlyBombs recebe o mapa atual e devolve apenas a(s) String(s) das Bombas.

=Exemplo 
>>> onlyBombs ["#####","#   #","# # #","#   #","#####","! 1 2","0 1 2 !"]
[]
>>> onlyBombs ["#####","#   #","# # #","#   #","#####","! 1 2","* 1 2 0 1 10","0 1 2 !"]
["* 1 2 0 1 10"]
-}  
onlyBombs :: [String] -> [String]
onlyBombs [] = []
onlyBombs l = filter (\(h:t) -> (h == '*')) l
{- |A função ordBombs recebe a nova String da Bomba, caso um Jogador tenha posto a Bomba e a lista de Strings das Bombas que existiam no mapa, ordenando essa String na lista de Strings.

=Exemplo 
>>> ordBombs "* 3 1 0 1 10" ["* 1 2 0 1 8"]
["* 1 2 0 1 8","* 3 1 0 1 10"]
>>> "* 3 1 0 1 10" ["* 1 2 0 1 8","* 5 3 0 1 5"]
["* 1 2 0 1 8","* 3 1 0 1 10","* 5 3 0 1 5"]
-}  
ordBombs :: String -> [String] -> [String]
ordBombs l1 [] = [l1]
ordBombs [] l2 = l2
ordBombs l1 l2 = ordena l1 l2
        where ordena l1 [] = [l1]
              ordena l1 (y:ys) | (((snd $ posPu l1 '*') <= (snd $ posPu y '*')) && ((fst $ posPu l1 '*') <= (fst $ posPu y '*'))) = (l1 : (y:ys))
                               | otherwise = y : (ordena l1 ys)
{- |A função onlyPlayers recebe o mapa atual e devolve apenas a(s) String(s) dos Jogadores. 

=Exemplo 
>>> onlyPlayers ["#####","#   #","# # #","#   #","#####","! 1 2","* 1 2 0 1 10","0 1 2 !"]
["0 1 2 !"]
>>> onlyPlayers ["#####","#   #","# # #","#   #","#####","! 1 2","* 1 2 0 1 10","0 1 2 !","1 3 2"]
["0 1 2 !","1 3 2"]
-}                               
onlyPlayers :: [String] -> [String]
onlyPlayers [] = []
onlyPlayers l = filter (\(h:t) -> (h /= '#' && h /= '+' && h /= '!' && h /= '*')) l
{- |A função move recebe o mapa atual, o jogador que irá efetuar o comando e esse mesmo comando, devolvendo o novo estado do mapa.

=Exemplo 
>>> putStr ( unlines(move ["#######","#     #","# # # #","# ?   #","# # # #","#     #","#######","+ 1 3","0 1 4","1 1 4 !"] 0 'U' ))
#######
#     #
# # # #
# ?   #
# # # #
#     #
#######
0 1 3 +
1 1 4 !
-}    
move1 :: [String] -> Int -> Char -> [String]
move1 m j p = if isAlive m j then auxMove m j p 
              else m

                       where auxMove m j p = if p == 'B'
                                             then onlyMapa m ++ onlyPu m ++ (ordBombs (canBomb a m j (temFlames m j) (near (takeL m a) a)) (onlyBombs m)) ++ onlyPlayers m
                                             else pickPu (changePos m j (canMove a p (near (takeL (putOnMap (onlyMapa m)(m \\ onlyMapa m)) a) a))) j
                             a = pickPlayer m j 
{- |A função isAlive recebe o estado atual de jogo e o identificador do jogador e verifica se esse jogador está vivo.

=Exemplo 
>>> isAlive ["#########","#  ??   #","# # # # #","#??? ?  #","# #?#?#?#","# ?   ??#","# #?#?# #","#       #","#########","0 3 4"] 0
True
>>> isAlive ["#########","#  ??   #","# # # # #","#??? ?  #","# #?#?#?#","# ?   ??#","# #?#?# #","#       #","#########","0 3 4"] 1
False
-} 
isAlive :: [String] -> Int -> Bool --ve se o jogador p esta vivo
isAlive [] _ = False
isAlive (h:t) p = if ([head h] == (show p)) then True else isAlive t p 



--Tarefa 4


{- |A função putOnMap recebe o onlyMapa e o estado de jogo sem o onlyMapa, e acrescenta no onlyMapa os Power Ups, as Bombas e os Jogadores.

=Exemplo 
>>> putStr(unlines(putOnMap ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 7","0 7 1","1 7 7"]))
#########
#*     0#
# #?#?# #
#  ?  ? #
#?# # #?#
# ?  ?  #
# #?#?# #
#  ??  1#
#########
-} 
putOnMap :: Mapa -> [String] -> [String]
putOnMap [] _ = []
putOnMap l [] = l
putOnMap m (h:t) = putOnMap (putAux h m (posBomb h)) t
    where putAux :: String -> [String] -> Position -> [String]
          putAux (h:t) (m1:m) (x,y) | y == 0 = (putAux2 h m1 x) : m
                                    | otherwise = m1 : putAux (h:t) m (x,y-1)

          putAux2 h (x:xs) p | p == 0 = if x == '?' then (x:xs) else h:xs
                             | otherwise = x : putAux2 h xs (p-1)
{- |A função pickTime recebe uma String (correspondente a uma Bomba) e devolve o tempo que essa bomba tem até explodir. 

=Exemplo 
>>> pickTime "* 7 6 0 1 1")
1
>>> pickTime "* 7 6 0 1 10")
10
-} 
pickTime :: String -> Int
pickTime l = read (words l !! 5)
{- |A função takeTime recebe a lista de Strings correspondente às bombas (proveniente da função onlyBombs) e retira todas as bombas que tem apenas 1 instante de tempo para explodir.

=Exemplo 
>>> takeTime ["* 7 7 1 3 1","* 7 6 0 1 8"]
["* 7 6 0 1 7"]
>>> takeTime ["* 7 7 1 3 2","* 7 6 0 1 8","* 9 8 1 4 6"]
["* 7 7 1 3 1","* 7 6 0 1 7","* 9 8 1 4 5"]
-} 
takeTime :: [String] -> [String] 
takeTime [] = []
takeTime (h:t) | pickTime h == 1 = takeTime t
               | otherwise = (aux h) : (takeTime t)
    where aux l = (unwords (take 5 (words l)))++ " " ++ (show (pickTime l - 1))
{- |A função numFlame recebe a String correspondente à bomba e devolve o raio de ação da bomba.

=Exemplo 
>>> numFlame "* 7 7 0 2 8"
2
>>> numFlame "* 74 71 0 15 10"
15
-} 
numFlame :: String -> Int 
numFlame l = read (words l !! 4)
{- |A função numFlames recebe a lista de String correspondente às bombas e devolve o raio de ação de cada bomba.

=Exemplo 
>>> numFlames ["* 7 7 0 2 8","* 74 71 0 15 10"]
[2,15]
-} 
numFlames :: [String] -> [Int]
numFlames [] = []
numFlames (h:t) = numFlame h : numFlames t
{- |A função posBomb recebe a String da bomba e devolve a posição desta.

=Exemplo 
>>> posBomb "* 7 7 0 2 8"
(7,7)
>>> posBomb "* 74 71 0 15 10"
(74,71)
-}
posBomb :: String -> Position
posBomb l = (read(words l !! 1), read(words l !! 2))
{- |A função posBombs recebe a lista de String das bombas e devolve as posições destas.

=Exemplo 
>>> posBombs ["* 7 7 0 2 8","* 74 71 0 15 10"]
[(7,7),(74,71)]
-}
posBombs :: [String] -> [Position]
posBombs [] = []
posBombs (h:t) = posBomb h : posBombs t
{- |A função whichBomb recebe a lista de String das bombas e devolve a lista de bombas que tem uma unidade de tempo.

=Exemplo 
>>> whichBomb ["* 7 7 0 2 8","* 74 71 0 15 10"]
[]
>>> whichBomb ["* 7 7 0 2 8","* 74 71 0 15 1"]
["* 74 71 0 15 1"]
>>> whichBomb ["* 7 7 0 2 1","* 74 71 0 15 1"]
["* 7 7 0 2 1","* 74 71 0 15 1"]
-}
whichBomb :: [String] -> [String]
whichBomb [] = []
whichBomb (h:t) = if (pickTime h) == 1 then h : whichBomb t else whichBomb t
{- |A função alteraH recebe o onlyMapa do estado atual (resultante de se ter feito a função putOnMap), a posição da bomba e o raio de ação que esta tem, devolvendo o novo estado de jogo, alterado apenas horizontalmente.

=Exemplo 
>>> putStr(unlines(alteraH ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] (3,1) 1))
#########
#  X    #
# #?#?# #
#  ?  ? #
#?# # #?#
# ?  ?  #
# #?#?# #
#  ??   #
#########
>>> putStr(unlines(alteraH ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] (1,3) 1))
#########
#       #
# #?#?# #
#X    ? #
#?# # #?#
# ?  ?  #
# #?#?# #
#  ??   #
#########
-} 
alteraH :: Mapa -> Position -> Int -> Mapa
alteraH (h:t) (x,y) f | y == 0 = (reverse (destroi (reverse (take x h)) f) ++ ['X'] ++ destroi (drop (x+1) h) f) : t 
                      | otherwise = h : alteraH t (x,y-1) f 
            where destroi [] f = []
                  destroi l (0) = l 
                  destroi (h:t) f | h == 'X' = h : destroi t (f-1)
                                  | h == '#' = '#' : t
                                  | (h == '+' || h == '!') = 'X' : t
                                  | (h == '0' || h == '1' || h == '2' || h == '3') = 'X' : destroi t (f-1)
                                  | h == '*' = 'X' : t
                                  | h == ' ' = h : destroi t (f-1)
                                  | otherwise = ' ' : t
{- |A função alteraV recebe o onlyMapa do estado atual (resultante de se ter feito a função putOnMap), a posição da bomba e o raio de ação que esta tem, devolvendo o novo estado de jogo, alterado apenas verticalmente.

=Exemplo 
>>> putStr(unlines(alteraV ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] (3,1) 1))
#########
#       #
# # #?# #
#  ?  ? #
#?# # #?#
# ?  ?  #
# #?#?# #
#  ??   #
#########
>>> putStr(unlines(alteraV ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] (1,3) 1))
#########
#       #
# #?#?# #
#  ?  ? #
# # # #?#
# ?  ?  #
# #?#?# #
#  ??   #
#########
-} 
alteraV :: Mapa -> Position -> Int -> Mapa
alteraV l (x,y) f = reverse(muda(reverse (take y l)) (x,y) f) ++ [l !! y] ++ (muda (drop (y+1) l) (x,y) f)
    where canExplode (h:t) | h == 'X' = h : t
                           | h == '#' = '#' :t
                           | (h == '+' || h == '!') = 'X' : t
                           | (h == '0' || h == '1' || h == '2' || h == '3') = 'X' : t
                           | h == '*' = 'X' : t
                           | h == ' ' = ' ' : t
                           | otherwise = ' ' : t 
          muda [] _ _ = []
          muda (h:t) (x,y) f | f == 0 = (h:t)
                             | (head (drop x h)) == ' ' || ((head (take x h))) == ' ' ||(head (drop x h)) == '0' || ((head (take x h))) == '0' || (head (drop x h)) == '1' || ((head (take x h))) == '1' || (head (drop x h)) == '2' || ((head (take x h))) == '2' || (head (drop x h)) == '3' || ((head (take x h))) == '3' = ((canExplode (take x h)) ++ (canExplode (drop x h))) : muda t (x,y) (f-1) 
                             | otherwise = ((canExplode (take x h)) ++ (canExplode (drop x h))) : t 
{- |A função altera recebe o mapa proveniente da função putOnMap, a lista de posições das bombas e a lista dos raios de ação que estas tem, devolvendo o novo estado de jogo, devido à explosão ou não de bombas.

=Exemplo
>>> putStr(unlines(altera ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] [(3,1)] [1]))
#########
#  X    #
# # #?# #
#  ?  ? #
#?# # #?#
# ?  ?  #
# #?#?# #
#  ??   #
#########
>>> putStr(unlines(altera ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] [(1,3)] [1]))
#########
#       #
# #?#?# #
#X    ? #
# # # #?#
# ?  ?  #
# #?#?# #
#  ??   #
#########
-}
altera :: Mapa -> [Position] -> [Int] -> Mapa
altera l [] [] = l 
altera l (h:t) (f1:f2) = altera (alteraV (alteraH l h f1) h f1) t f2
{- |A função reaper recebe apenas o mapa do novo estado de jogo proveniente da função altera e o resto do estado de jogo sem o onlyMapa, e um par de inteiros que servirá como contador, retirando aquilo que foi eliminado devido à explosão da bomba na lista dos Power Ups, das Bombas e dos Jogadores.

=Exemplo
>>> reaper ["#####","#X  #","# # #","#  1#","#####"] ["0 1 1","1 3 3"] (0,0)
["1 3 3"]
>>> reaper ["#####","#0 X#","# # #","#  1#","#####"] ["+ 3 1","0 1 1","1 3 3"] (0,0)
["0 1 1","1 3 3"]
-}
reaper :: Mapa -> [String] -> Position -> [String] -- position contador (0,0)
reaper l [] _ = []
reaper [] l _ = l
reaper (x:xs) l (h,v) = reaper xs (aux x l (h,v)) (h,v+1)
                    where aux :: String -> [String] -> Position -> [String]
                          aux [] l _ = l
                          aux (h:t) l (x,y) | h == 'X' =  aux t (aux2 l (x,y)) (x+1,y)
                                            | h == '#' =  aux t (aux3 l (x,y)) (x+1,y)
                                            | otherwise = aux t l (x+1,y)
                                            
                          aux2 :: [String] -> Position -> [String]
                          aux2 [] _ = []
                          aux2 (h:t) (x,y) | (head h) == '*' && posBomb h == (x,y) = (unwords((take 5 (words h)) ++ [show 1])) : aux2 t (x,y)
                                           | posBomb h == (x,y) = aux2 t (x,y)
                                           | otherwise = h:(aux2 t (x,y))


                          aux3 :: [String] -> Position -> [String]
                          aux3 [] _ = []
                          aux3 (h:t) (x,y) | (head h) == '*' && posBomb h == (x,y) =  aux2 t (x,y)
                                           | posBomb h == (x,y) = aux2 t (x,y)
                                           | otherwise = h:(aux2 t (x,y))      


{- |A função clean vai apenas à parte do mapa (onlyMapa) e retira tudo o que foi adicionado na função putOnMap.

=Exemplo 
>>> clean ["#####","#0 X#","# # #","#  1#","#####"]
["#####","#   #","# # #","#   #","#####"]
>>> clean ["#######","#0    #","#X# # #","#X??  #","# #?# #","#    1#","#######"]caracol
["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######"]
-} 
clean :: Mapa -> Mapa
clean [] = []
clean (h:t) = (aux h) : (clean t)
    where aux [] = []
          aux (h:t) | ((h /= '?') && (h /= '#')) = ' ' : aux t
                    | otherwise = h : aux t
{- |A função caracol reproduz o "efeito caracol" que ocorre quando falta pouco tempo para acabar a partida. A função caracol recebe o estado de jogo atual e um contador que inicia no 0.

=Exemplo
>>> putStr(unlines(caracol ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######"] 0))
#######
##    #
# # # #
# ??  #
# #?# #
#     #
#######
>>> putStr(unlines(caracol ["#######","#######","# # ###","# ?? ##","# #?###","#   ###","#######"] 0))
#######
#######
# # ###
# ?? ##
# #?###
#  ####
#######
-} 
caracol :: Mapa -> Int -> Mapa -- Int contador == 0
caracol [] _ = []
caracol l n |  (filter (not .null ) $ map ( filter (/='#')) l) == [] = l
            | (isfull $ l !! n) == False = (take n l) ++ [(aux $ l !! n)] ++ (drop (n+1) l) 
            | (isLast l n) == False = putLast l n
            | (isfull $ (reverse l) !! n) == False = (take (length l -n-1) l) ++ [reverse $ aux (reverse (l !! (length l - n-1)))] ++ (drop (length l - n) l) 
            | (isFirst l n) == False = reverse(putFirst (reverse l) n)
            | otherwise = caracol l (n+1)
  where isfull [] = True -- vê se uma string é toda de #
        isfull (h:t) = h == '#' && isfull t

        aux [] = [] -- poe um # na 2 string 
        aux (h:t) = if h == '#' then h: aux t else '#' : t
     
        putLast [] _ = [] -- mete numa lista de strings um #
        putLast (h:t) n = if ((reverse h) !! n) == '#' then h:putLast t n else reverse (aux (reverse h)) : t

        isLast [] _ = True  -- vê se uma [String] tem o penúltimo char preenchido
        isLast (h:t) n = if ((reverse h) !! n) == '#' then isLast t n else False

        isFirst [] _ = True
        isFirst (h:t) n = if (h !! n) == '#' then isFirst t n else False

        putFirst [] _ = []
        putFirst (h:t) n = if (h !! n) == '#' then h : putFirst t n else (aux h) : t
{- |A função avanca1 recebe o estado de jogo atual e o número instantes de tempo que faltam para o jogo terminar, devolvendo o novo estado de jogo resultante da passagem de tempo.

=Exemplo 
>>> putStr $ unlines $ avanca1 ["#########","#       #","# # # # #","#?   ? ?#","#?# # #?#","# ?   ? #","# # #?# #","#    ?  #","#########","+ 5 7","* 2 3 0 1 1","0 1 1"] 55
#########
#       #
# # # # #
#    ? ?#
#?# # #?#
# ?   ? #
# # #?# #
#    ?  #
#########
+ 5 7
0 1 1
>>> putStr $ unlines $ avanca1 ["#########","#       #","# # # # #","#?   ? ?#","#?# # #?#","# ?   ? #","# # #?# #","#    ?  #","#########","+ 5 7","* 3 3 0 1 1","* 3 4 0 1 10","0 3 2"] 55
#########
#       #
# # # # #
#?   ? ?#
#?# # #?#
# ?   ? #
# # #?# #
#    ?  #
#########
+ 5 7
* 3 4 0 1 1
>>> putStr $ unlines $ avanca1 ["#########","#       #","# # # # #","#?   ? ?#","#?# # #?#","# ?   ? #","# # #?# #","#    ?  #","#########","+ 5 7","0 1 1"] 49
#########
##      #
# # # # #
#?   ? ?#
#?# # #?#
# ?   ? #
# # #?# #
#    ?  #
#########
+ 5 7
-}
avanca1 :: [String] -> Int -> [String]
avanca1 l t | t <= ((length(head l)-2)^2 ) = (clean a) ++ (reaper a (onlyPu l) (0,0)) ++ (reaper a (takeTime (onlyBombs l)) (0,0)) ++ (reaper a (onlyPlayers l) (0,0))
            | otherwise = (clean b) ++ (reaper b (onlyPu l) (0,0)) ++ (reaper b (takeTime (onlyBombs l)) (0,0)) ++ (reaper b (onlyPlayers l) (0,0))

     where a = altera (caracol (putOnMap (onlyMapa l) (l \\ (onlyMapa l))) 0) (posBombs (whichBomb (onlyBombs l))) (numFlames (whichBomb (onlyBombs l)))
           b = altera (putOnMap (onlyMapa l) (l \\ (onlyMapa l))) (posBombs (whichBomb (onlyBombs l))) (numFlames (whichBomb (onlyBombs l)))

{- =heading Tarefa 6
-}


{- |A função dangerOnMap recebe o mapa do estado atual de jogo e a lista das bombas, devolvendo o mapa do estado de jogo com os sítios que cada bomba atingirá com a explosão marcados por um 'Z'.

=Exemplo 
>>> putStr $ unlines $ dangerOnMap ["#########","#       #","# # # # #","#?   ? ?#","#?# # #?#","# ?   ? #","# # #?# #","#    ?  #","#########"] ["* 1 1 0 0 10"]
#########
#*Z     #
#Z# # # #
#?   ? ?#
#?# # #?#
# ?   ? #
# # #?# #
#    ?  #
#########
>>> putStr $ unlines $ dangerOnMap ["#########","#       #","# # # # #","#?   ? ?#","#?# # #?#","# ?   ? #","# # #?# #","#    ?  #","#########"] ["* 3 3 0 1 10"]
#########
#  Z    #
# #Z# # #
#?Z*Z? ?#
#?#Z# #?#
# ?Z  ? #
# # #?# #
#    ?  #
#########
-}
dangerOnMap :: Mapa -> [String] -> Mapa -- marca os sitios que a bomba atinge com as chamas
dangerOnMap [] _ = []
dangerOnMap l [] = l
dangerOnMap m (h:t) = dangerOnMap (putAux h m (posBomb h)) t
    where putAux :: String -> [String] -> Position -> [String]
          putAux _ [] _ = []
          putAux (h:t) m (x,y) = reverse (aux2 (reverse (take y m)) x (numFlame (h:t)+1)) ++ [((reverse(aux h (reverse(take x (m!!y))) (numFlame (h:t)+1))) ++ "*" ++ (aux h (drop (x+1) (m!!y)) (numFlame (h:t)+1)))] ++ (aux2 (drop (y+1) m) x (numFlame (h:t)+1))
                                  
          aux :: Char -> String -> Int -> String
          aux _ [] _ = []
          aux h (x:xs) n | n == 1 = if x == '?' || x == '#' then (x:xs) else ('Z':xs)
                         | otherwise = if x == '?' || x == '#' then (x:xs) else 'Z' : aux h xs (n-1)

          aux2 :: Mapa -> Int -> Int -> Mapa 
          aux2 [] _ _ = []
          aux2 l _ (0) = l
          aux2 (h:t) x n | (h!!x) == '?' || (h!!x) == '#' = (h:t)
                         | otherwise = aux3 h x : aux2 t x (n-1)

          aux3 :: String -> Int -> String
          aux3 h x = take x h ++ "Z" ++ drop (x+1) h