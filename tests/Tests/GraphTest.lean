import Graph
import Std.Data.HashMap


def graph := ({ adjacency := Std.HashMap.ofList [("a", ["b", "c"]), ("b", ["c"]), ("c", ["d"])]} : DiGraph)


namespace TestAddEdge

  /-- info: { adjacency := Std.HashMap.ofList [("a", [])] } -/
  #guard_msgs in
  #eval DiGraph.empty {} |>.addEdge "a" none


  /-- info: { adjacency := Std.HashMap.ofList [("c", ["d"]), ("a", ["b", "c", "e"]), ("b", ["c"])] } -/
  #guard_msgs in
  #eval graph.addEdge "a" (some "e") 


  /-- info: { adjacency := Std.HashMap.ofList [("e", []), ("c", ["d"]), ("a", ["b", "c"]), ("b", ["c"])] } -/
  #guard_msgs in
  #eval graph.addEdges "e" none

end TestAddEdge


namespace TestaddEdges

  /-- info: { adjacency := Std.HashMap.ofList [("c", ["d"]), ("a", ["b", "c", "e", "f"]), ("b", ["c"])] } -/
  #guard_msgs in
  #eval graph |>.addEdges "a" (some ["e", "f"]) 

end TestaddEdges


namespace TestDegree

  /-- info: 4 -/
  #guard_msgs in
  #eval graph.degree

end TestDegree


namespace TestDepthFirstSearch

  /-- info: some (Std.HashSet.ofList ["d", "c", "a", "b"]) -/
  #guard_msgs in
  #eval graph.depthFirstSearch "a"


  /-- info: some (Std.HashSet.ofList ["d", "c", "b"]) -/
  #guard_msgs in
  #eval graph.depthFirstSearch "b"

end TestDepthFirstSearch

