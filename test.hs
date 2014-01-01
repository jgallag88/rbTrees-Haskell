import qualified Data.List as L
import RBTree
import qualified Data.Set as S

main = putStrLn $ show $ member 123352 $ L.foldl' (flip insert) (Empty B) [1 .. 1000000]

