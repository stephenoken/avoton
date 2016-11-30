

-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "Dijkstra's algorithm" $ do
    it "returns the shortest path between 2 points" $ do
      putStrLn . show  $ createWGraph
      dijkstra createWGraph 1  4 `shouldBe` ([1,2,3,4], 1.0)
      dijkstra createWGraph 1  7 `shouldBe` ([1,2,6,7], 1.0)



createWGraph :: Wgraph
createWGraph = Wgraph [
  Wedge (Edge (1,2),3.1),
  Wedge (Edge (1,5),2.6),
  Wedge (Edge (1,6),9.2),
  Wedge (Edge (2,6),2.1),
  Wedge (Edge (2,3),4.4),
  Wedge (Edge (3,4),1.2),
  Wedge (Edge (3,6),3.1),
  Wedge (Edge (3,7),2.3),
  Wedge (Edge (5,6),6.4),
  Wedge (Edge (4,7),2.5),
  Wedge (Edge (6,7),1.1)
  ]
