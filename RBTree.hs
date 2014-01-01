--Red-Black Tree implementation in Haskell, based largely on Chris Okasaki's implementation

{-# LANGUAGE ViewPatterns #-}

module RBTree where

data Color = R | B | BB deriving (Show, Eq)
data Tree a = Empty Color
              | Node Color !(Tree a) a !(Tree a) deriving (Show, Eq)

empty = Empty B

insert :: Ord a => a -> Tree a -> Tree a
insert x tree = toBlk $ ins tree
        where ins (Empty _) = Node R (Empty B) x (Empty B)
              ins n@(Node c lTree val rTree)
                  | x >= val = balance $ Node c lTree val (ins rTree)
                  | x < val  = balance $ Node c (ins lTree) val rTree

-- Use rotation and re-painting to regain red-black property when we come across any case
-- of a red node with a black parent and red child.
balance :: Tree a -> Tree a
balance (Node B (Node R a x (Node R b y c)) z d) = (Node R (Node B a x b) y (Node B c z d))
balance (Node B (Node R (Node R a x b) y c) z d) = (Node R (Node B a x b) y (Node B c z d))
balance (Node B a x (Node R (Node R b y c) z d)) = (Node R (Node B a x b) y (Node B c z d))
balance (Node B a x (Node R b y (Node R c z d))) = (Node R (Node B a x b) y (Node B c z d))
balance t = t

delete :: Ord a => a -> Tree a -> Tree a
delete x tree = toBlk $ delfrom tree
    where delfrom (Empty B) = Empty B                         -- x is not in tree
          delfrom (Node c lTree val rTree)
              | x > val = rebalanceR $ Node c lTree val (delfrom rTree)
              | x < val  = rebalanceL $ Node c (delfrom lTree) val rTree
          delfrom (Node c lTree val rTree@(Node _ _ _ _)) =   -- x is internal node
              let (rTree', succ') = delMin rTree
              in rebalanceR $ Node c lTree succ' rTree'
          delfrom n@(Node c lTree val rTree@(Empty _)) =      -- x is leaf node
              let (newTree, _) = delMin n
              in newTree

--Returns minimum element from tree, along with tree with minimum element removed.
--For use in delete function, so tree returned may not be a proper red black tree
--(it may have a red or double-black root).
delMin :: Tree a -> (Tree a, a)
delMin (Node B (Empty B) min (Empty B)) = (Empty BB, min) 
delMin (Node R (Empty B) min (Empty B)) = (Empty B, min)
delMin (Node B (Empty B) min (Node R r x l)) = (Node B r x l, min)
delMin (Node c l val r) = let (l', min) = delMin l
                          in (rebalanceL $ Node c l' val r, min)

--Rebalance after deletion. Use view patterns to match nodes which might be leaves
--Case 3: let double black bubble up
rebalanceL :: Tree a -> Tree a
rebalanceL (Node B n@(clr -> BB) p (Node R sl s sr)) =                        --Case 2 
    (Node B (rebalanceL (Node R n p sl)) s sr)
rebalanceL (Node B n@(clr -> BB) p (Node B sl@(clr -> B) s sr@(clr -> B))) =  --Case 3
    (Node BB (toBlk n) p (Node R sl s sr)) 
rebalanceL (Node R n@(clr -> BB) p (Node B sl@(clr -> B) s sr@(clr -> B))) =  --Case 4
    (Node B (toBlk n) p (Node R sl s sr)) 
rebalanceL (Node c n@(clr -> BB) p (Node B sl s (Node R srl sr srr))) =       --Case 6
    (Node c (Node B (toBlk n) p sl) s (Node B srl sr srr))
rebalanceL (Node c n@(clr -> BB) p (Node B (Node R sll sl slr) s sr)) =       --Case 5
    (Node c (Node B (toBlk n) p sll) sl (Node B slr s sr)) 
rebalanceL n = n 

rebalanceR :: Tree a -> Tree a
rebalanceR (Node B (Node R sl s sr) p n@(clr -> BB)) =                        --Case 2 
    (Node B sl s (rebalanceR (Node R n p sr)))
rebalanceR (Node B (Node B sl@(clr -> B) s sr@(clr -> B)) p n@(clr -> BB)) =  --Case 3
    (Node BB (Node R sl s sr) p (toBlk n)) 
rebalanceR (Node R (Node B sl@(clr -> B) s sr@(clr -> B)) p n@(clr -> BB)) =  --Case 4
    (Node B (Node R sl s sr) p (toBlk n)) 
rebalanceR (Node c (Node B (Node R sll sl slr) s sr) p n@(clr -> BB)) =       --Case 6
    (Node c (Node B sll sl slr) s (Node B sr p (toBlk n)))
rebalanceR (Node c (Node B sl s (Node R srl sr srr)) p n@(clr -> BB)) =       --Case 5
    (Node c (Node B sl s srl) sr (Node B srr p (toBlk n))) 
rebalanceR n = n

--Get color of node. This, along with 'toBlk' allow us to treat leaves and other nodes at the 
-- the same time when rebalancing
clr (Empty c) = c
clr (Node c _ _ _) = c

toBlk (Empty _) = Empty B
toBlk (Node _ l val r) = Node B l val r

member :: Ord a => a -> Tree a -> Bool
member _ (Empty _) = False
member x (Node _ lTree val rTree)
    | x < val = member x lTree
    | x > val = member x rTree
    | otherwise = True

--Pretty printer
pprint :: Show a => Tree a -> IO () 
pprint = putStrLn . concat . prnt
    where prnt (Empty _) = ["----NIL\n"] 
                         --["\n", "----NIL\n", "\n"] Nicer spacing but too much room
          prnt (Node c lTree value rTree) =
                     (preSpace (prnt rTree) ' ' '|') ++ 
                     ["----" ++ "(" ++ show c ++ ") " ++ show value ++ "\n"] ++ 
                     (preSpace (prnt lTree) '|' ' ')
          preSpace :: [String] -> Char -> Char -> [String]
          preSpace []            _  _  = []
          preSpace (l@('-':_):ls) c1 c2 = ("      " ++ "|"  ++ l) : preSpace ls c2 c1
          preSpace (l:ls) c1 c2         = ("      " ++ [c1] ++ l) : preSpace ls c1 c2

checkInvariant :: Tree a -> Bool
checkInvariant t = check t 0 
    where check (Node R (Node R _ _ _) _ _) blkH = False
          check (Node R _ _ (Node R _ _ _)) blkH = False
          check (Node R l _ r) blkH = (check l blkH) && (check r blkH)
          check (Node B l _ r) blkH = (check l $ blkH + 1) && (check r $ blkH + 1) 
          check (Empty _) blkH = blkH == blkHeight
          blkHeight = bh t 0
          bh (Node B l _ _) height = bh l $ height + 1
          bh (Node R l _ _) height = bh l height
          bh (Empty _)      height = height
