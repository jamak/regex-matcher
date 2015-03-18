import System.Environment

data Regex = Sequence Regex Regex | Repetition Regex | Primitive Char | Any | Empty deriving Show

build :: String -> Regex
build "" = Empty
build ('.':cs) = Sequence Any (build cs)
build (c:'*':cs) = Sequence (Repetition (Primitive c)) (build cs)
build (c:cs) = Sequence (Primitive c) (build cs)

main = do 
          rgx:str <- getArgs
          print "hey"
