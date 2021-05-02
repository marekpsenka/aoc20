import qualified Data.Set as Set

type V3 = (Int, Int, Int)

v3Add :: V3 -> V3 -> V3
v3Add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

neighborhood :: V3 -> Set.Set V3
neighborhood v = Set.fromList $ [v3Add v (i, j, k) | i <- [-1..1], j <- [-1..1], k <- [-1..1]]

envelope :: Set.Set V3 -> Set.Set V3
envelope s = Set.unions $ Set.map neighborhood s

rule :: Bool -> Int -> Bool
rule act actNeighbors
  | act = actNeighbors == 2 || actNeighbors == 3
  | actNeighbors == 3 = True
  | otherwise = False

main :: IO ()
main = do
  inpt <- lines <$> getContents

  let indices = [(i, j) | i <- [0..length inpt - 1], j <- [0..length (head inpt) - 1]]
      b = zip indices (concat inpt)
      a = Set.fromList $ map ((\(x, y) -> (x, y, 0)) . fst) (filter (('#'==). snd) b)

  print a
  print (Set.size $ neighborhood (0, 0, 0))

