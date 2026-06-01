import Graph
import Std

open Std


def graph := ({ adjacency := HashMap.ofList [("a", ["b", "c"]), ("b", ["c"]), ("c", ["d"])]} : DiGraph)



/-- info: { adjacency := Std.HashMap.ofList [("a", [])] } -/
#guard_msgs in
#eval DiGraph.empty {} |>.addEdge "a" none


/-- info: { adjacency := Std.HashMap.ofList [("c", ["d"]), ("a", ["b", "c", "e"]), ("b", ["c"])] } -/
#guard_msgs in
#eval graph.addEdge "a" (some "e") 


/-- info: { adjacency := Std.HashMap.ofList [("e", []), ("c", ["d"]), ("a", ["b", "c"]), ("b", ["c"])] } -/
#guard_msgs in
#eval graph.addEdges "e" none




/-- info: { adjacency := Std.HashMap.ofList [("c", ["d"]), ("a", ["b", "c", "e", "f"]), ("b", ["c"])] } -/
#guard_msgs in
#eval graph |>.addEdges "a" (some ["e", "f"]) 



/-- info: { adjacency := Std.HashMap.ofList [("d", ["c"]), ("c", ["a", "b"]), ("b", ["a"])] } -/
#guard_msgs in
#eval graph.reverseEdges



/-- info: "DiGraph {\nc -> [d]\na -> [b, c]\nb -> [c]\n}" -/
#guard_msgs in
#eval graph.toString



/-- info: "  \"b\" -> \"c\"\n  \"a\" -> \"b\"\n  \"a\" -> \"c\"\n  \"c\" -> \"d\"" -/
#guard_msgs in
#eval graph.toDotNodes



/-- info: 4 -/
#guard_msgs in
#eval graph.degree



/-- info: some (Std.HashSet.ofList ["d", "c", "a", "b"]) -/
#guard_msgs in
#eval graph.depthFirstSearch "a"


/-- info: some (Std.HashSet.ofList ["d", "c", "b"]) -/
#guard_msgs in
#eval graph.depthFirstSearch "b"



/-- info: some (Std.HashSet.ofList ["d", "c"]) -/
#guard_msgs in
#eval graph.findSuccessors "b"



/-- info: some (Std.HashSet.ofList ["a", "b"]) -/
#guard_msgs in
#eval graph.findPredecessors "c"


/-- info: { adjacency := Std.HashMap.ofList [("a", ["b"]), ("b", [])] } -/
#guard_msgs in
#eval graph.getSubGraph {"a", "b"}
