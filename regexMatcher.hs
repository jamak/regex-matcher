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
match (Repetition r1)     "" = True
match _                   "" = False
match (Sequence r1 Empty) s  = match r1 s
-- match (Sequence r1 r2)    s  = case (r1,r2) of 
--                                                ((Repetition Any),_)
match (Sequence r1 r2)    s  = match r1 (takeWhile (not .matchChar r2)  s) && match r2 (head $ dropWhile (match r1) $ iterate tail s)
match (Primitive c)       s  = [c] == s

matchChar :: Regex -> Char -> Bool
matchChar Any _  = True
matchChar Empty _ = False
matchChar (Primitive r) c = r == c 
matchChar (Sequence r1 Empty) s = matchChar r1 s
matchChar (Sequence r1 r2) s = (matchChar r1 s) && (matchChar r2 s)
matchChar (Repetition Any) _ = False

main :: IO ()
main = do 
          rgx:str:_ <- getArgs
          print $ match (build rgx) str
