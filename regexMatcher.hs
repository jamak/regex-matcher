import System.Environment

data Regex = Sequence Regex Regex | Repetition Regex | Primitive Char | Any | Empty deriving Show

build :: String -> Regex
build "" = Empty
build ('.':'*':cs) = Sequence (Repetition Any) (build cs)
build ('.':cs) = Sequence Any (build cs)
build (c:'*':cs) = Sequence (Repetition (Primitive c)) (build cs)
build (c:cs) = Sequence (Primitive c) (build cs)

match :: Regex -> String -> Bool
match Any _              = True
match Empty _            = True
match (Primitive c) s    = [c] == s
match (Sequence r1 r2) s = or [(match r1 u1) && (match r2 u2) | (u1,u2) <- split s]
match (Repetition r1) s  = or $ (match r1 s):[and [match r1 s1 | s1 <- ps] | ps <- partitions s]

-- charMatch :: String -> String -> Bool
-- charMatch s p = case (x@(head s),y@(head p)) of
--                     x == '.' -> True
--                     head x == '*' -> 

-- All possible ways to split a list in two
split :: [a] -> [([a], [a])]
split []     = [([], [])]
split (c:cs) = ([], c:cs):[(c: s1, s2) | (s1, s2) <- split cs]

-- returns a list of all the the possible ways to split up the list
partitions :: [a] -> [[[a]]]
partitions []     = [[]]
partitions [c]    = [[[c]]]
partitions (c:cs) = concat [[(c:p):ps, [c]:p:ps]| p:ps <- partitions cs]

main :: IO ()
main = do 
          rgx:str:_ <- getArgs
          print $ match (build rgx) str
