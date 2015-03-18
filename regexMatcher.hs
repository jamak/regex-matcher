import System.Environment

data Regex = Sequence Regex Regex | Repetition Regex | Primitive Char | Any | Empty deriving Show

build :: String -> Regex
build "" = Empty
build ('.':cs) = Sequence Any (build cs)
build (c:'*':cs) = Sequence (Repetition (Primitive c)) (build cs)
build (c:cs) = Sequence (Primitive c) (build cs)

match :: Regex -> String -> Bool
match Any                 _  = True
match Empty               _  = False
match _                   "" = False
match (Sequence r1 Empty) s  = match r1 s
match (Sequence r1 r2)    s  = match r1 s && match r2 (head $ dropWhile (match r1) $ iterate tail s)
match (Repetition r1)     s  = any (match r1) (take (length s) $ iterate tail s)
match (Primitive c)       s  = c == head s

main :: IO ()
main = do 
          rgx:str:_ <- getArgs
          print $ match (build rgx) str
