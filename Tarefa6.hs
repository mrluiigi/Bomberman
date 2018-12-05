{- | 
Module : Tarefa6
Description : Tarefa6
Copyright : José Pinto,Luís Correia;
-}
module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import Tarefa6_li1g034 (bot)
import Bomberman
{- | O programa main pode ser usado para testar a função bot, que após compilado, aceita como parâmetros o identificador do jogador e o número de instantes de tempo que faltam para o jogo
terminar, ficando à espera do estado do jogo no stdin e invocando a função bot , imprimindo o resultado no stdout.
-}
main :: IO ()
main = do
    a <- getArgs
    let player = readMaybe (a !! 0)
    let ticks = readMaybe (a !! 1)
    w <- getContents
    if isJust player && isJust ticks
        then putStr $ show $ bot (lines w) (fromJust player) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"