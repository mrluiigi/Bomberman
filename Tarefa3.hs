{- | 
Module : Tarefa 3
Description : Tarefa3
Copyright : José Pinto,Luís Correia;
-}
module Main where
    
import System.Environment
import Data.List
{- |A função encode recebe o estado do mapa e comprime-o.

=Exemplo 
>>> encode ["#########","#       #","# #?# #?#","#        #","# # # # #","#        #","# # # # #","#       #","#########","0 1 1","1 3 2"]
"7\n ? ?\n8\n    \n8\n    \n7\nA11\nB32\n"
-}
encode :: [String] -> String
encode l | length (head l) <= 9  = unlines ((remSide (bigSpaceToInt (remUpDown (onlyMapa l)))) ++ takeSpaces (onlyPu l) ++ (onlyBombs l) ++ takeSpaces (changeNPlayer (onlyPlayers l)))
         | otherwise = unlines ((remSide (bigSpaceToInt (remUpDown (onlyMapa l)))) ++ (onlyPu l) ++ (onlyBombs l) ++ (changeNPlayer (onlyPlayers l)))
{- |A função onlyMapa recebe o estado de jogo atual e devolve apenas o mapa sem os Power Ups, as bombas e os jogadores.

=Exemplo 
>>> onlyMapa ["#####","#   #","# # #","#   #","#####","! 1 2","* 1 1 0 1 5","0 1 2 !"]
["#####","#   #","# # #","#   #","#####"]
-}
onlyMapa :: [String] -> [String]
onlyMapa [] = []
onlyMapa l = filter (\(h:t) -> (h == '#' || h == ' ' || h == '?')) l 
{- |A função onlyPu recebe o estado de jogo atual e devolve apenas as linhas do jogo onde estão listados os Power Ups.

=Exemplo 
>>> onlyPu ["#####","#   #","# # #","#   #","#####","! 1 2","* 1 1 0 1 5","0 1 2 !"]
["! 1 2"]
-}
onlyPu :: [String] -> [String]
onlyPu [] = []
onlyPu l = filter (\(h:t) -> (h == '+' || h == '!')) l 
{- |A função onlyBombs recebe o estado de jogo atual e devolve apenas as linhas do jogo onde estão listadas as Bombas.

=Exemplo 
>>> onlyBombs ["#####","#   #","# # #","#   #","#####","! 1 2","* 1 1 0 1 5","0 1 2 !"]
["* 1 1 0 1 5"]
-}
onlyBombs :: [String] -> [String]
onlyBombs [] = []
onlyBombs l = filter (\(h:t) -> (h == '*')) l 
{- |A função onlyPlayers recebe o estado de jogo atual e devolve apenas as linhas do jogo onde estão listados os jogadores e as suas posições.

=Exemplo 
>>> onlyPlayers ["#####","#   #","# # #","#   #","#####","! 1 2","* 1 1 0 1 5","0 1 2 !"]
["0 1 2 !"]
-}
onlyPlayers :: [String] -> [String]
onlyPlayers [] = []
onlyPlayers l = filter (\(h:t) -> (h == '0' || h == '1' || h == '2' || h == '3')) l 
{- |A função changeNPlayer recebe as Strings dos jogadores e troca o número do jogador por uma letra.

=Exemplo 
>>> changeNPlayer ["0 1 1","1 2 2","2 3 3","3 4 4"]
["A 1 1","B 2 2","C 3 3","D 4 4"]
-}
changeNPlayer :: [String] -> [String]
changeNPlayer [] = []
changeNPlayer ((x:xs):t) | x == '0' = ('A':xs) : changeNPlayer t
                         | x == '1' = ('B':xs) : changeNPlayer t
                         | x == '2' = ('C':xs) : changeNPlayer t
                         | x == '3' = ('D':xs) : changeNPlayer t
{- |A função remUpDown recebe a lista de Strings proveniente da função onlyMapa (apenas o mapa) e devolve esse mapa sem a primeira e a última linha.

=Exemplo 
>>> remUpDown ["#####","#   #","# # #","#   #","#####"]
["#   #","# # #","#   #"]
>>> remUpDown ["#######","#     #","# # # #","# ?   #","# #?# #","#     #","#######"]
["#     #","# # # #","# ?   #","# #?# #","#     #"]
-}
remUpDown :: [String] -> [String]
remUpDown l = init (tail l)
{- |A função remSide recebe a lista de Strings proveniente da função remUpDown e retira de cada String (linha do mapa) o primeiro e o último cardinal (pedra).

=Exemplo 
>>> remSide ["#   #","# # #","#   #"]
["   ","  ","   "]
>>> remSide ["#     #","# # # #","# ?   #","# #?# #","#     #"]
["     ","   "," ?   "," ? ","     "]
-}
remSide :: [String] -> [String]
remSide [] = []
remSide (h:t) = filter (/= '#') h : (remSide t)
{- |A função delSpace recebe uma String e retira os espaços que existem nessa String.

=Exemplo 
>>> delSpace "0 1 1"
"011"
>>> delSpace "+ 34 100"
"+34100"
-}
delSpace :: String -> String
delSpace l = filter (/= ' ') l
{- |A função takeSpaces recebe uma lista de String e retira os espaços que existem em cada String dessa lista.

=Exemplo 
>>> takeSpaces ["+ 1 1","! 2 2","* 1 1 0 1 10","0 1 1"]
["+11","!22","*110110","011"]
-}
takeSpaces:: [String] -> [String]
takeSpaces [] = []
takeSpaces (h:t) = (delSpace h) : (takeSpaces t)
{- |A função spaceToInt recebe uma String e se esta tiver espaços seguidos, retira-os e conta-os.

=Exemplo 
>>> spaceToInt "#    #"
"#4#"
>>> spaceToInt "#    ?  #"
"#4?2#"
-}
spaceToInt :: String -> String 
spaceToInt [] = []
spaceToInt [x] = [x]
spaceToInt (h:t) | h == ' ' = show (aux (h:t)) ++ spaceToInt (drop (aux (h:t)-1) t)
                 | otherwise = h : spaceToInt t
            where aux [] = 0
                  aux (h:t) = if h == ' ' then 1 + aux t else 0        
{- |A função bigSpaceToInt recebe uma lista de Strings e a cada String (exceto as que tiverem pedra intercalada), se esta tiver espaços seguidos, retira-os e conta-os.

=Exemplo 
>>> bigSpaceToInt ["#  ?#","# # #","#   #"]
["#2?#","# # #","#3#"]
-}
bigSpaceToInt :: [String] -> [String]
bigSpaceToInt [] = []
bigSpaceToInt [x] = [spaceToInt x]
bigSpaceToInt (h:m:t) = spaceToInt h : m : bigSpaceToInt t

{- |A função addSpace recebe uma String (neste caso, correspondente a Power Ups ou a um Jogador) e adiciona espaços de modo a reconstruir a String inicial

=Exemplo 
>>> addSpace "+11"
"+ 1 1"
>>> addSpace "012"
"0 1 2"
-}
addSpace :: String -> String
addSpace (h:t:s) = h : ' ' : aux (t:s)
    where aux [] = []
          aux [x] = [x]
          aux (t:s) = if (t == '+' || t == '!') then (t:s) else t : ' ' : aux s
{- |A função putSpaces recebe uma lista de Strings correspondente às linhas dos Power Ups e dos Jogadores e adiciona os espaços a cada String de modo a reconstruir a lista de Strings inicial.

=Exemplo 
>>> putSpaces ["+11","012"]
["+ 1 1","0 1 2"]
-}
putSpaces :: [String] -> [String]
putSpaces [] = []
putSpaces (h:t) = addSpace h : putSpaces t
{- |A função intToSpaces faz o inverso da função spaceToInt, ou seja, recebe uma String que contém os espaços contados (representados por um número) e transforma esse número em espaços.

=Exemplo 
>>> intToSpaces ["4?2"]
["    ?  "]
-}
intToSpaces :: [String] -> [String]
intToSpaces [] = []
intToSpaces ((x:xs):t) | (x /= '+') && (x /= '!') && (x /= '*') && (x /= 'A') && (x /= 'B') && (x /= 'C') && (x /= 'D') = aux (x:xs) : intToSpaces t
                       | otherwise = (x:xs) : intToSpaces t
                where aux [] = []
                      aux (h:t) | (h == ' ') || (h == '?') = h : aux t
                                | otherwise = (replicate (read [h]) ' ') ++ (aux t) 
{- |A função onlyPlayersD recebe o estado de jogo comprimido e devolve apenas as linhas do jogo onde estão listados os jogadores e as suas posições.

=Exemplo 
>>> onlyPlayersD (lines "7\n ? ?\n8\n    \n8\n    \n7\nA11\nB32\n")
["A11","B32"]
-}
onlyPlayersD :: [String] -> [String]
onlyPlayersD [] = []
onlyPlayersD l = filter (\(h:t) -> (h == 'A' || h == 'B' || h == 'C' || h == 'D')) l 
{- |A função playerNChange recebe uma lista de Strings dos jogadores e troca a letra que foi atribuída ao jogador pelo seu número inicial.

=Exemplo 
>>> playerNChange ["A11","B32"]
["011","132"]
-}
playerNChange :: [String] -> [String]
playerNChange [] = []
playerNChange ((x:xs):t) | x == 'A' = ('0':xs) : playerNChange t
                         | x == 'B' = ('1':xs) : playerNChange t
                         | x == 'C' = ('2':xs) : playerNChange t
                         | x == 'D' = ('3':xs) : playerNChange t
                         | otherwise = (x:xs) : playerNChange t
{- |A função dim recebe apenas o mapa do estado de jogo comprimido e calcula a dimensão do mapa.

=Exemplo 
>>> dim ["7"," ? ?","8","    ","8","    ","7","A11","B32"]
9
-}
dim :: [String] -> Int -- calcula a dimensão do mapa
dim [] = 0
dim (h:m:t) = (length m) * 2 + 1
{- |A função addCar recebe apenas o mapa do estado de jogo comprimido e a cada String da lista de Strings, adiciona as pedras que tinham sido inicialmente retiradas.

=Exemplo 
>>> addCar ["       "," ? ?","        ","    ","        ","    ","       "]
["       "," #?# #?","        "," # # # ","        "," # # # ","       "]
-}
addCar :: [String] -> [String]
addCar [] = []
addCar [x] = [x]
addCar (h:m:t)= h : (intersperse '#' m) : addCar t
{- |A função decode recebe o estado do mapa comprimido e descomprime-o, de modo a reconstituir o mapa inicial.

=Exemplo 
>>> decode "7\n ? ?\n8\n    \n8\n    \n7\nA11\nB32\n"
["#########","#       #","# #?# #?#","#        #","# # # # #","#        #","# # # # #","#       #","#########","0 1 1","1 3 2"]
-}
decode :: String -> [String]
decode l | dim (lines l) <= 9 = [replicate (dim (lines l)) '#'] ++ (map (\l->('#': l ++ "#")) (addCar (onlyMapa (intToSpaces (lines l))))) ++ [replicate (dim (lines l)) '#'] ++ putSpaces (onlyPu (lines l)) ++ (onlyBombs (lines l)) ++ (putSpaces ( playerNChange (onlyPlayersD (lines l))))
         | otherwise = [replicate (dim (lines l)) '#'] ++ (map (\l->('#': l ++ "#")) (addCar (onlyMapa (intToSpaces (lines l))))) ++ [replicate (dim (lines l)) '#'] ++ (onlyPu (lines l)) ++ (onlyBombs (lines l)) ++ (playerNChange(onlyPlayersD (lines l)))
{- |O programa main é usado para testar as funçoes encode e decode, que após compilado, aceita como parâmetros a opção
-e (para codificar) e -d (para descodificar), ficando à espera do estado do jogo (normal ou
comprimido) no stdin, invocando a função encode (ou decode , dependendo da opção), imprimindo o resultado no stdout .
-}
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"