{- | 
Module : Tarefa 2
Description : Tarefa2
Copyright : José Pinto,Luís Correia;
-}
module Main where
  
import Bomberman
import Data.Char (isDigit)
import System.Environment

move :: [String] -> Int -> Char -> [String]
move m j p = move1 m j p 


{- |O programa main é usado para testar a função move:
após compilado, o programa aceita como parâmetros o identificador de um jogador e um comando, fica à espera do estado do jogo no
stdin e invoca a função move , imprimindo o resultado no stdout.
-} 
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"