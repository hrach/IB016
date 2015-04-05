{- | This is the second assignment for IB015, semester spring 2015.
  Name: Jan Skrasek
  UID: 373816

  === Additional documentation / useful links

  *   <http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Set.html Data.Set>

  *   <http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Map-Lazy.html Data.Map>

  *   <http://hackage.haskell.org/package/base-4.7.0.2/docs/Data-List.html Data.List>


  === Points
  You should implement all the functions as documented below. All functions
  except for 'shortestPath' are for 10 points altogether. The remaining 10 point are 
  for 'shortestPath'. You can get some bonus points for nice implementation
  or multiple algorithms.
 -}

module Graph (
    -- * Data types
      Vertex (..)
    , Edge (..)
    , Graph (..)
    -- * Construction and data manipulation helpers
    , graph
    , addVertex
    , addEdge
    , deleteVertex
    , deleteEdge
    -- * Basic querying functions
    , findVertex
    , findOutgoing
    , vertexCount
    , edgeCount
    , totalWeight
    , averageWeight
    , medianWeight
    -- * Graph search
    , shortestPath
    -- * Test graph
    , testGraph
    ) where

import Data.List
import Data.Set ( Set )
import Data.Map ( Map )
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map as M

-- | Vertex of graph.
newtype Vertex = Vertex Integer deriving (Eq, Ord, Show, Read)

-- | Oriented edge.
data Edge = Edge { from :: Vertex, to :: Vertex } deriving (Eq, Ord, Show, Read)

-- | Graph, it contains set of vertices, and a associative map from edges to
-- edge values (which behaves like set with additionally associated edge value).
data Graph = Graph { vertices :: Set Vertex, edges :: Map Edge Integer }
             deriving (Eq, Show, Read)

-- | Pre-defined graph for testing purposes.
-- Corresponds to graph displayed in <https://en.wikipedia.org/wiki/Dijkstra's_algorithm>
testGraph :: Graph
testGraph = Graph (S.fromList $ map Vertex [1..6]) (M.fromList $ concatMap makeEdges edges)
  where makeEdges (from, to, weight) = [ (Edge (Vertex from) (Vertex to), weight)
                                       , (Edge (Vertex to) (Vertex from), weight)]
        edges = [ (1, 2, 7)
                , (1, 3, 9)
                , (1, 6, 14)
                , (2, 3, 10)
                , (2, 4, 15)
                , (3, 4, 11)
                , (3, 6, 2)
                , (4, 5, 6)
                , (5, 6, 9)
                ]

-- | Create empty graph.9 d
graph :: Graph
graph = Graph S.empty M.empty

-- | Add a vertex to the graph, if it is already present, it will be overridden.
addVertex :: Vertex -> Graph -> Graph
addVertex v (Graph vs es) = Graph (S.insert v vs) es

-- | Add an edge with given value to the graph, if the edge is present already
-- it will be overridden. If any of the edge's vertices are not in graph
-- they will be added.
addEdge :: Edge -> Integer -> Graph -> Graph
addEdge (Edge f t) w (Graph vs es) = Graph (S.insert t (S.insert f vs)) (M.insert (Edge f t) w es)

-- | Delete vertex from graph, together will all edges which contain it.
-- If vertex is not present, nothing function has no effect.
deleteVertex :: Vertex -> Graph -> Graph
deleteVertex v (Graph vs es) = Graph (S.delete v vs) (M.filterWithKey (\(Edge f t) _ -> (f == v || t == v)) es)

-- | Delete edge from graph, does not modify vertex set. If edge is not present,
-- function has no effect.
deleteEdge :: Edge -> Graph -> Graph
deleteEdge e (Graph vs es) = Graph vs (M.delete e es)

-- | Find all edges containing given vertex and return map from those edges to
-- their value
-- 
-- >>> findVertex (Vertex 5) testGraph
-- fromList [(Edge {from = Vertex 4, to = Vertex 5},6),
--           (Edge {from = Vertex 5, to = Vertex 4},6),
--           (Edge {from = Vertex 5, to = Vertex 6},9),
--           (Edge {from = Vertex 6, to = Vertex 5},9)]
findVertex :: Vertex -> Graph -> Map Edge Integer
findVertex v (Graph _ es) = M.filterWithKey (\(Edge f t) _ -> f == v || t == v) es

-- | Like 'findVertex' but finds all outgoing edges of given vertex.
-- 
-- >>> findOutgoing (Vertex 5) testGraph
-- fromList [(Edge {from = Vertex 5, to = Vertex 4},6),
--           (Edge {from = Vertex 5, to = Vertex 6},9)]
findOutgoing :: Vertex -> Graph -> Map Edge Integer
findOutgoing v (Graph _ es) = M.filterWithKey (\k _ -> from k == v) es

-- | Calculate number of vertices in graph.
-- 
-- >>> vertexCount testGraph
-- 6
vertexCount :: Graph -> Int
vertexCount (Graph vs _) = S.size vs

-- | Calculate number of edges in graph.
-- 
-- >>> edgeCount testGraph
-- 18
edgeCount :: Graph -> Int
edgeCount (Graph _ es) = M.size es

-- | Sum of all edge values.
--
-- >>> totalWeight testGraph
-- 166
totalWeight :: Graph -> Integer
totalWeight (Graph _ es) = sum $ M.toList es

-- | Average value of edges, if no edges are present 'Nothing' is returned.
-- Bonus: calculate average in one pass
--
-- >>> averageWeight testGraph
-- Just 9.222222222222221
averageWeight :: Fractional a => Graph -> Maybe a
averageWeight (Graph _ es) = if not $ M.null es
                             then Just ((fromIntegral $ sum edges) / (fromIntegral $ length edges))
                             else Nothing
                             where edges = M.toList es

-- | Median value of edges, if no edges are present 'Nothing' is returned.
-- In case of even number of edges, use integer division to obtain median.
--
-- >>> medianWeight testGraph
-- Just 9
medianWeight :: Graph -> Maybe Integer
medianWeight (Graph _ es) = if M.null es
                            then Nothing
                            else
                                if odd count
                                then Just (edges !! (count `div` 2))
                                else Just (edges !! (count `div` 2) - 1)
                            where edges = sort $ M.toList es
                                  count = length edges

{- | Find shortest path from one vertex to another. If there is no such path,
  or vertices are not present, return 'Nothing'.
  You can use one of:

  * Dijkstra: <https://en.wikipedia.org/wiki/Dijkstra's_algorithm>

  * Bellman-Ford: <https://en.wikipedia.org/wiki/Bellman-Ford_algorithm>

  * Floyd-Warshall: <https://en.wikipedia.org/wiki/Floyd-Warshall_algorithm>


  Dijkstra's algorithm is the fastest if properly implemented, but its implementation
  is also the hardest. You can assume there are no edges with negative values. However,
  if the algorithm of your choice can handle them, you can detect them and return
  'Nothing' in such case.

  You can also get bonus points if you implement multiple algorithms. Just name
  them @spDijkstra@ // @spFloydWarshall@ // @spBellmanFord@ and use 'shortestPath'
  as an alias to one of them.

  >>> shortestPath (Vertex 1) (Vertex 5) testGraph
  Just 20
-}
shortestPath :: Vertex -> Vertex -> Graph -> Maybe Integer
shortestPath from to g = if M.member to d then d M.! to else Nothing 
                         where d = distances from g

distances :: Vertex -> Graph -> Map Vertex (Maybe Integer)
distances from g = relax ((S.size $ vertices g) - 1) (M.toList $ edges g) (M.insert from (Just 0) d)
                   where d = M.fromList $ zip (S.toList $ vertices g) (repeat Nothing)

relax :: Int -> [(Edge, Integer)] -> Map Vertex (Maybe Integer) -> Map Vertex (Maybe Integer)
relax 0 _ distances  = distances
relax n es distances = relax (n - 1) es (subrelax es distances)

subrelax :: [(Edge, Integer)] -> Map Vertex (Maybe Integer) -> Map Vertex (Maybe Integer)
subrelax [] distances = distances
subrelax ((edge,w):es) distances = M.insert (to edge) (distance edge w d) d 
                                   where d = subrelax es distances

distance :: Edge -> Integer -> Map Vertex (Maybe Integer) -> Maybe Integer
distance (Edge from to) w distances = maybeMin ((+w) <$> (distances M.! from)) (distances M.! to)

maybeMin :: Maybe Integer -> Maybe Integer -> Maybe Integer 
maybeMin (Just a) (Just b) = Just (min a b)
maybeMin (Just a) _ = Just a
maybeMin _ b = b
