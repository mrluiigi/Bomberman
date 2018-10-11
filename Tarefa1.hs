{- | 
Module : Tarefa 1
Description : Mapa Bomberman
Copyright : Luís Correia, José Pinto;
-}
module Main where

import System.Random
import System.Environment
import Text.Read
import Data.Maybe
import System.IO
import Bomberman

mapa :: Int -> Int -> [String]
mapa d s = mapa1 d s
{-|O programa main aceita como parâmetros a dimensão e a semente e depois invoca
a função ​ mapa ​ imprimindo o resultado no stdout.                
-}
main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"