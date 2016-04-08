module Ast where
import Data.Ratio
import Data.Complex
import Data.Array
import Data.IORef
import Control.Monad.Except
import Text.ParserCombinators.Parsec
import System.IO

type Env = IORef [(String, IORef LispVal)]

data LispVal = Nil
             | String String
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: Maybe String,
                     body :: [LispVal], closure :: Env}
             | IOFunc([LispVal] -> IOThrowsError LispVal)
             | Port Handle

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "#\\" ++ [c]
showVal (Float f) = show f
showVal (Complex comp) = (show.realPart) comp ++ " + " ++ (show.imagPart) comp ++ "i"
showVal (Ratio ratio) = (show.numerator) ratio ++ "/" ++ (show.denominator) ratio
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList first rest) = "(" ++ unwordsList first ++ " . " ++ showVal rest ++ ")"
showVal (Vector arry) = "#(" ++ (unwords . map show $ elems arry) ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func{params=args, vararg=varargs, body=body, closure=env} =
     "(lambda (" ++ unwords args ++
         (case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                        ++ " args; found " ++ unwordsList found
showError (TypeMismatch expected found) = "Type mismatch: expected " ++ show expected
                                        ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = show message

instance Show LispError where show = showError

-- instance Error LispError where
--     noMsg = Default "An error has occurred"
--     strMsg = Default

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right value) = value
