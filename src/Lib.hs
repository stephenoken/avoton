module Lib
    ( someFunc,
      Node,
      Edge(..),
      Wedge(..),
      Wnode(..),
      Wgraph(..),
      Source,
      Destination,
      dijkstra
    ) where

-- types
type Node = Int
type Source = Int
type Destination = Int

data Edge = Edge (Node,Node) deriving (Show)
data Wedge = Wedge (Edge, Float) deriving (Show)
data Wnode = Wnode { node :: Node, pre :: Node, dist :: Float} deriving (Show, Eq)
data Wgraph = Wgraph [Wedge] deriving (Show)





dijkstra :: Wgraph -> Source -> Destination -> ([Int],Float)
dijkstra g u v = ([1,2,3], 6.4)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
