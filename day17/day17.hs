import qualified Data.Set as Set

type V3 = (Int, Int, Int)
type V4 = (Int, Int, Int, Int)

v3Add :: V3 -> V3 -> V3
v3Add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

v4Add :: V4 -> V4 -> V4
v4Add (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1 + x2, y1 + y2, z1 + z2, w1 + w2)

neighborhood :: V3 -> Set.Set V3
neighborhood v = Set.delete v full
  where
    full = Set.fromList $ [v3Add v (i, j, k) | i <- [-1..1], j <- [-1..1], k <- [-1..1]]

neighborhood4 :: V4 -> Set.Set V4
neighborhood4 v = Set.delete v full
  where
    full = Set.fromList $ [v4Add v (i, j, k, l) | i <- [-1..1], j <- [-1..1], k <- [-1..1], l <- [-1..1]]

envelope :: Set.Set V3 -> Set.Set V3
envelope s = Set.unions $ Set.map neighborhood s

envelope4 :: Set.Set V4 -> Set.Set V4
envelope4 s = Set.unions $ Set.map neighborhood4 s

rule :: Bool -> Int -> Bool
rule act actNeighbors
  | act = actNeighbors == 2 || actNeighbors == 3
  | actNeighbors == 3 = True
  | otherwise = False

numActiveNeighbors :: V3 -> Set.Set V3 -> Int
numActiveNeighbors v active = Set.size (Set.intersection (neighborhood v) active)

numActiveNeighbors4 :: V4 -> Set.Set V4 -> Int
numActiveNeighbors4 v active = Set.size (Set.intersection (neighborhood4 v) active)

transition :: Set.Set V3 -> Set.Set V3
transition active = Set.union (Set.difference active (Set.fromList toDie)) (Set.fromList toLive)
  where
    toConsider = Set.toList (Set.union (envelope active) active)
    trComponent v = rule (Set.member v active) (numActiveNeighbors v active)
    trMap = zip toConsider (map trComponent toConsider)
    toLive = map fst (filter snd trMap)
    toDie = map fst (filter (not . snd) trMap)

transition4 :: Set.Set V4 -> Set.Set V4
transition4 active = Set.union (Set.difference active (Set.fromList toDie)) (Set.fromList toLive)
  where
    toConsider = Set.toList (Set.union (envelope4 active) active)
    trComponent v = rule (Set.member v active) (numActiveNeighbors4 v active)
    trMap = zip toConsider (map trComponent toConsider)
    toLive = map fst (filter snd trMap)
    toDie = map fst (filter (not . snd) trMap)


main :: IO ()
main = do
  inpt <- lines <$> getContents

  let indices = [(i, j) | i <- [0..length inpt - 1], j <- [0..length (head inpt) - 1]]
      indexed = zip indices (concat inpt)
      a = Set.fromList $ map ((\(x, y) -> (x, y, 0)) . fst) (filter (('#'==). snd) indexed)
      b = Set.fromList $ map ((\(x, y) -> (x, y, 0, 0)) . fst) (filter (('#'==). snd) indexed)
      final = iterate transition a !! 6
      final4 = iterate transition4 b !! 6

  print (Set.size final)
  print (Set.size final4)

