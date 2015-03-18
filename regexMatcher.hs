import System.Environment

data Regex = Sequence Regex Regex | Repetition Regex | Primitive Char | Any | Empty deriving Show

build :: String -> Regex
build "" = Empty
build ('.':'*':cs) = Sequence (Repetition Any) (build cs)
build ('.':cs) = Sequence Any (build cs)
build (c:'*':cs) = Sequence (Repetition (Primitive c)) (build cs)
build (c:cs) = Sequence (Primitive c) (build cs)

match :: Regex -> String -> Bool
match Any                 _  = True
match Empty               _  = False
match (Repetition r1)     s  = any (match r1) (take (length s) $ iterate tail s)
match r@(Sequence (Repetition r1) r2) s = match r2 s || ((null s) && charMatch r1 (head s) && match r (tail s))
match r@(Primitive c) s = charMatch r (head s)
match _                   "" = False
match (Sequence r1 Empty) s  = match r1 s
match (Sequence r1 r2)    s  = match r1 (takeWhile (not .charMatch r2)  s) && match r2 (head $ dropWhile (match r1) $ iterate tail s)

charMatch :: Regex -> Char -> Bool
charMatch Any _  = True
charMatch Empty _ = False
charMatch (Primitive r) c = r == c 
-- charMatch (Sequence r1 Empty) s = charMatch r1 s
-- -- charMatch (Sequence r1 r2) s = (charMatch r1 s) && (charMatch r2 s)
-- charMatch (Repetition Any) _ = False

-- charMatch :: String -> String -> Bool
-- charMatch s p = case (x@(head s),y@(head p)) of
--                     x == '.' -> True
--                     head x == '*' -> 

main :: IO ()
main = do 
          rgx:str:_ <- getArgs
          print $ match (build rgx) str
