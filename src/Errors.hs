module Errors where

import Control.Monad.Except
import Ast

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
