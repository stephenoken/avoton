module Lib
    ( Node,
      Edge(..),
      Wedge(..),
      Wnode(..),
      Wgraph(..),
      Source,
      Destination,
      dijkstra,
      pathToNode,
      distToNode
    ) where

import Data.List
import Data.Maybe
import Data.Function

-- types
type Node = Int
type Source = Int
type Destination = Int

newtype Edge = Edge (Node,Node) deriving (Show)
newtype Wedge = Wedge (Edge, Float) deriving (Show)
data Wnode = Wnode { node :: Node, pre :: Node, dist :: Float} deriving (Show, Eq)
newtype Wgraph = Wgraph [Wedge] deriving (Show)

---------------------------
-- General Helper fucntions
---------------------------

-- List of weighted edges for a weighted graph
edges :: Wgraph -> [Wedge]
edges (Wgraph xs) = xs

-- List of nodes for a weighted edges
enodes :: Wedge -> [Node]
enodes (Wedge (Edge(e1,e2),_)) = [e1,e2]

-- List of all nodes for a list of weighted edges
nodesForWedges :: [Wedge] -> [Node]
nodesForWedges = nub . concatMap enodes

-- List of all nodes for a graph
nodes :: Wgraph -> [Node]
nodes = nodesForWedges . edges

-- The weight of a weighted edge
weight :: Wedge -> Float
weight (Wedge(_,w)) = w

-- Given a node and a graph, get a list of weighted edges incident on the node
incidentWedges :: Node -> Wgraph -> [Wedge]
incidentWedges node = filter (\e -> node `elem` enodes e) . edges

-- Given a node and a list of weighted nodes, get the corresponding weighted node
wnodeForNode :: Node -> [Wnode] -> Wnode
wnodeForNode n = head . filter (\wn -> node wn == n)

-- Given a couple of nodes and a graph, try to find a weighted edge incident on the nodes
tryGetWedge :: Wgraph -> Node -> Node -> Maybe Wedge
tryGetWedge (Wgraph es) n1 n2 = find (\x -> [n1,n2] \\ enodes x == []) es

-- Given a weighted node and maybe a weighted edge,
-- return the weighted node if we were given an edge and the node is in the edge -- otherwise Nothing
tryGetWnode :: Wnode -> Maybe Wedge -> Maybe Wnode
tryGetWnode wnode Nothing = Nothing
tryGetWnode wnode (Just wedge)
  | node wnode `elem` enodes wedge = Just wnode
  | otherwise = Nothing
------------------
-- Initialisation
------------------

-- Bootstrap the dijkstraAlg and
dijkstra :: Wgraph -> Source -> [Wnode]
dijkstra g start =
  let wnodes  = initWnodes start g
      curNode = Just (wnodeForNode start wnodes)
      checked = []
      (_,_,_,wnodes') = dijkstraAlg g checked curNode wnodes
  in wnodes'

wnodeForStart :: Node -> Source -> Wgraph -> Wnode
wnodeForStart node start graph
  | node == start = Wnode {node = node, pre = node, dist = 0}
  | isNothing maybeWedge  = Wnode {node = node, pre = start, dist = infinity}
  | otherwise = Wnode {node = node, pre = start, dist = weight . fromJust $ maybeWedge}
  where
    infinity = 1.0/0.0 :: Float
    maybeWedge = tryGetWedge graph node start

initWnodes :: Source -> Wgraph -> [Wnode]
initWnodes u g = map (\n -> wnodeForStart n u g) . nodes $ g

-------------------
-- Main Algorithm
-------------------

-- Recursively perform Dijkstra's algorithm until all nodes have been checked
dijkstraAlg :: Wgraph -> [Node] -> Maybe Wnode -> [Wnode] -> (Wgraph, [Node], Maybe Wnode, [Wnode])
dijkstraAlg g checked Nothing wnodes = (g, checked, Nothing, wnodes)
dijkstraAlg g checked (Just curNode) wnodes =
  let cn = node curNode
      checked' = cn : checked
      incidents = incidentWedges cn g
      wnodes' = updateWnodes g curNode incidents wnodes
      unChecked = unchecked checked' wnodes'
      curNode' = minimalUnchecked unChecked
  in dijkstraAlg g checked' curNode' wnodes'

-- Given a list of Wnodes and a list of checked nodes, return a list of unchecked Wnodes
unchecked :: [Node] -> [Wnode] -> [Wnode]
unchecked  checked wnodes = filter (\x -> not $ (node x) `elem` checked) wnodes

-- Given a list of unchecked Wnodes, maybe return the minimal weighted node
minimalUnchecked :: [Wnode] -> Maybe Wnode
minimalUnchecked [] = Nothing
minimalUnchecked wnodes = Just . minimumBy (compare `on` dist) $ wnodes
-- Given a base node and the weighted edges incident on it, Update the list of weighted nodes
updateWnodes :: Wgraph -> Wnode -> [Wedge] -> [Wnode] ->[Wnode]
updateWnodes g curNode incidents = map (updateWnode g curNode incidents)

-- Given a base node, the edges incident on it and a weighted node, return a (possibly) updated weighted node
updateWnode :: Wgraph ->  Wnode -> [Wedge] -> Wnode -> Wnode
updateWnode g curNode incidents wnode
  | tryGetWnode wnode mWedge == Nothing = wnode
  | otherwise = updateConnected curNode mWedge wnode
  where mWedge = tryGetWedge g (node curNode) (node wnode)

-- Given a base node an edge and a connected weighted node, return an updated weighted node
updateConnected :: Wnode -> Maybe  Wedge -> Wnode -> Wnode
updateConnected curNode (Just wedge) wnode =
  let ext = (dist curNode) + (weight wedge)
      improved  = ext < (dist wnode)
  in Wnode {
    node = node wnode,
    pre = if improved then node curNode else pre wnode,
    dist = min ext (dist wnode)
    }

----------------------
-- EXTRACTING RESULTS
----------------------

-- returns path to a node
pathToNode :: [Wnode] -> Node -> [Node]
pathToNode wnodes start = reverse . map (node) . pathToWnode wnodes . wnodeForNode start $ wnodes

-- Return a path to a wighted node
pathToWnode :: [Wnode] -> Wnode -> [Wnode]
pathToWnode wnodes wnode
  | node wnode == pre wnode = [wnode]
  | otherwise = wnode : pathToWnode wnodes prenode'
  where prenode' = wnodeForNode (pre wnode) wnodes

-- Return distance to node
distToNode :: [Wnode] -> Node -> Float
distToNode wnodes node = dist $ wnodeForNode node  wnodes
