{- | 
Module : Tarefa4
Description : Tarefa4
Copyright : José Pinto,Luís Correia;
-}
module Main where

import Data.Char (isDigit)
import Data.List
import System.Environment
import Text.Read
import Data.Maybe
import Bomberman
{- | A função avanca recebe o estado atual do jogo e o número instantes de tempo que faltam para o jogo terminar, devolvendo o novo estado do jogo.

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
avanca :: [String] -> Int -> [String]
avanca l t = avanca1 l t
{- | O programa main pode ser usado para testar esta função. Após compilado, o programa aceita como parâmetro o número de instantes de tempo que faltam para o jogo terminar, ficando à espera do estado do jogo 
no stdin e invocando a função avanca, imprimindo o resultado no stdout.
-}
main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"