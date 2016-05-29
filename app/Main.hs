module Main where

import           Repl
import           System.Environment
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    if null args
        then runRepl
        else runOne args
