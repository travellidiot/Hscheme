{-# LANGUAGE ExistentialQuantification #-}

module Eval where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Except
import System.IO
import Data.Maybe
import Data.Array
import Ast
import SymbolTable
import Parser

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = fmap extractValue (runExceptT (trapError action))


readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right value -> return value

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces1)


ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String  filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = do
    str <- liftIO (readFile filename)
    liftThrows $ readExprList str

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env val@(Float _) = return val
eval env val@(Complex _) = return val
eval env val@(Ratio _) = return val
eval env val@(Vector _) = return val
eval env (Atom var) = getVar env var
eval env (List [Atom "quote", val]) = return val

eval env (List [Atom "if", preds, conseq, alt]) =
    do result <- eval env preds
       case result of
            Bool True -> eval env conseq
            Bool False -> eval env alt
            _ -> throwError $ TypeMismatch "boolean" result

eval env form@(List (Atom "cond" : clauses)) =
    if null clauses
    then throwError $ BadSpecialForm "Unrecognized special form" form
    else case head clauses of
        List [Atom "else", expr] -> eval env expr
        List [testCase, expr] -> eval env $ List [Atom "if",
                                              testCase,
                                              expr,
                                              List $ Atom "cond" : tail clauses]
        _ -> throwError $ TypeMismatch "boolean" form

eval env form@(List (Atom "case" : key : clauses)) =
    if null clauses
    then throwError $ BadSpecialForm "Unrecognized special form" form
    else case head clauses of
        List (Atom "else" : exprs) -> liftM last (mapM (eval env) exprs)
        List (List datums : exprs) -> do
            result <- eval env key
            equality <- mapM (\x -> liftThrows $ eqv [result, x]) datums
            if Bool True `elem` equality
            then liftM last $ mapM (eval env) exprs
            else eval env $ List (Atom "case" : key : tail clauses) where
                elem x@(Bool value) ((Bool v):[]) = value == v
                elem x@(Bool value) ((Bool v):xs) = if value == v
                                                    then True
                                                    else elem x xs
        _ -> throwError $ BadSpecialForm "Unrecognized special form" form

eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var: params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = do
    content <- load filename
    liftM last $ mapM (eval env) content
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
   if num params /= num args && isNothing varargs
       then throwError $ NumArgs (num params) args
       else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
   where
       remainingArgs = drop (length params) args
       num = toInteger . length
       evalBody env = liftM last $ mapM (eval env) body
       bindVarArgs args env = case args of
           Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
           Nothing -> return env

apply (IOFunc func) args = func args

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                                ++ map (makeFunc PrimitiveFunc) primitives)
   where makeFunc constructor (var, func) = (var, constructor func)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ (Func $ map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", unaryOp stringp),
              ("symbol?", unaryOp symbolp),
              ("number?", unaryOp numberp),
              ("list?", unaryOp listp),
              ("bool?", unaryOp boolp),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              ("<=", numBoolBinop (<=)),
              (">=", numBoolBinop (>=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string>=?", strBoolBinop (>=)),
              ("string<=?", strBoolBinop (<=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?",  eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _            [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params       = liftM (Number . foldl1 op) (mapM unpackNum params)


unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [item] = return $ f item
unaryOp _ _      = return $ Bool False

stringp, symbolp, numberp, listp, boolp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _          = Bool False
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool False
listp   _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False

string2symbol, symbol2string :: LispVal -> LispVal
string2symbol (String s) = Atom s
string2symbol _          = Atom ""
symbol2string (Atom a)   = String a
symbol2string _          = String ""

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ op left right

numBoolBinop= boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number num) = return num
unpackNum (String s)   = let parsed = reads s in
                            if null parsed
                            then throwError $ TypeMismatch "number" $ String s
                            else return $ fst $ head parsed
unpackNum (List [n])   = unpackNum n
unpackNum notNum       = throwError $ TypeMismatch "number" notNum

unpackIntList :: LispVal -> ThrowsError [Integer]
unpackIntList (List []) = return []
unpackIntList (List (x:xs)) = do
    head_ <- unpackNum x
    tail_ <- unpackIntList $ List xs
    return $ head_ : tail_
unpackIntList notList    = throwError $ TypeMismatch "list" notList

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b)   = return $ show b
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

------------------------
-- List Primitives -----
------------------------
car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgList           = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList _ x]      = return x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Number arg1), (Number arg2)]       = return $ Bool $ arg1 == arg2
eqv [(Bool arg1), (Bool arg2)]           = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]       = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]           = return $ Bool $ arg1 == arg2
eqv [(Ratio arg1), (Ratio arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Complex arg1), (Complex arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Character arg1), (Character arg2)] = return $ Bool $ arg1 == arg2
-- eqv [(Vector arg1), (Vector arg2)]       = return $ Bool $ arg1 == arg2
eqv [(Vector vec1), (Vector vec2)]       = eqv [List $ elems vec1, List $ elems vec2]
eqv [(List arg1), (List arg2)]           = return $ Bool $ (length arg1 == length arg2) &&
                                                           (all eqvPair $ zip arg1 arg2)
                                                        where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                                                    Left err -> False
                                                                                    Right (Bool value) -> value
eqv [(DottedList head1 tail1), (DottedList head2 tail2)] = eqv [List $ head1++[tail1], List $ head2++[tail2]]
eqv [_, _]                               = return $ Bool False
eqv badArgList                           = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
    `catchError` const (return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [(List lst1), (List lst2)] = return $ Bool $ (length lst1 == length lst2) &&
                                                   (all equalPair $ zip lst1 lst2)
                                                where equalPair (x1, x2) = case equal [x1, x2] of
                                                                            Left err -> False
                                                                            Right (Bool value) -> value
equal [arg1, arg2]               = do
                                primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                                                   [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
                                eqvEquals <- eqv [arg1, arg2]
                                return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList                 = throwError $ NumArgs 2 badArgList
