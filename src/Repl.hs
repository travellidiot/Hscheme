module Repl where

import           Control.Monad
import           System.Environment
import           System.IO

import           Ast
import           Eval
import           SymbolTable

flushStr :: String -> IO ()
flushStr prompt = putStr prompt >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    unless (pred result) $ action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (liftM show $ eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== ".q") (readPrompt "Hscheme|> ") . evalAndPrint
