

-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "Dijkstra's algorithm" $ do
    it "check the  distances between node 1 and the other nodes" $ do
      dijkstra graph 1  `shouldBe` weightedNodes

    it "returns the path between node 1 and node B" $ do
      let g = dijkstra graph 1
      pathToNode g 7 `shouldBe` [1,2,6,7]

    it "returns the distance between node 1 and node B" $ do
      let g = dijkstra graph 1
      distToNode g 7 `shouldBe` 6.2999997

weightedNodes :: [Wnode]
weightedNodes = [
  Wnode {node = 1, pre = 1, dist = 0.0},
  Wnode {node = 2, pre = 1, dist = 3.1},
  Wnode {node = 5, pre = 1, dist = 2.6},
  Wnode {node = 6, pre = 2, dist = 5.2},
  Wnode {node = 3, pre = 2, dist = 7.5},
  Wnode {node = 4, pre = 3, dist = 8.7},
  Wnode {node = 7, pre = 6, dist = 6.2999997}
  ]

graph :: Wgraph
graph = Wgraph [
  Wedge (Edge (1,2),3.1),
  Wedge (Edge (1,5),2.6),
  Wedge (Edge (1,6),9.2),
  Wedge (Edge (2,3),4.4),
  Wedge (Edge (2,6),2.1),
  Wedge (Edge (3,4),1.2),
  Wedge (Edge (3,6),3.1),
  Wedge (Edge (3,7),2.3),
  Wedge (Edge (4,7),2.5),
  Wedge (Edge (5,6),6.4),
  Wedge (Edge (6,7),1.1)
  ]
