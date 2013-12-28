--Red-Black Tree implementation in Haskell, based largely on Chris Okasaki's implementation

module RBTree where

data Color = R | B deriving (Show, Eq)
data Tree a = Empty
              | Node Color !(Tree a) a !(Tree a) deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x tree = Node B lTree val rTree
        where Node _ lTree val rTree = ins x tree
              ins x Empty = Node B Empty x Empty
              ins x (Node c lTree val rTree)
                  | lTree == Empty && x <  val = (Node c (Node R Empty x Empty) val rTree)
                  | rTree == Empty && x >= val = (Node c lTree val (Node R Empty x Empty))
                  | x >= val                   = balance $ Node c lTree val (ins x rTree)
                  | otherwise                  = balance $ Node c (ins x lTree) val rTree

-- Use rotation and re-painting to regain red-black property when we come across any case
-- of a red node with a black parent and red child.
balance :: Tree a -> Tree a
balance (Node B (Node R a x (Node R b y c)) z d) = (Node R (Node B a x b) y (Node B c z d))
balance (Node B (Node R (Node R a x b) y c) z d) = (Node R (Node B a x b) y (Node B c z d))
balance (Node B a x (Node R (Node R b y c) z d)) = (Node R (Node B a x b) y (Node B c z d))
balance (Node B a x (Node R b y (Node R c z d))) = (Node R (Node B a x b) y (Node B c z d))
balance t = t

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member x (Node _ lTree val rTree)
    | x < val = member x lTree
    | x > val = member x rTree
    | otherwise = True

--Pretty printer
pprint :: Show a => Tree a -> IO () 
pprint = putStrLn . concat . prnt
    where prnt Empty = ["----NIL\n"]  --["\n", "----NIL\n", "\n"] Nicer spacing but too much room
          prnt (Node c lTree value rTree) =
                     (preSpace (prnt rTree) ' ' '|') ++ 
                     ["----" ++ "(" ++ show c ++ ") " ++ show value ++ "\n"] ++ 
                     (preSpace (prnt lTree) '|' ' ')
          preSpace :: [String] -> Char -> Char -> [String]
          preSpace []            _  _  = []
          preSpace (l@(c:cs):ls) c1 c2 = if c == '-'
                                         then ("      " ++ "|"  ++ l) : preSpace ls c2 c1
                                         else ("      " ++ [c1] ++ l) : preSpace ls c1 c2

checkInvariant :: Tree a -> Bool
checkInvariant t = check t 0 
    where check (Node R (Node R _ _ _) _ _) blkH = False
          check (Node R _ _ (Node R _ _ _)) blkH = False
          check (Node R l _ r) blkH = (check l blkH) && (check r blkH)
          check (Node B l _ r) blkH = (check l $ blkH + 1) && (check r $ blkH + 1) 
          check Empty blkH = blkH == blkHeight
          blkHeight = bh t 0
          bh (Node B l _ _) height = bh l $ height + 1
          bh (Node R l _ _) height = bh l height
          bh Empty          height = height
