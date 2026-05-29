import Graph


/-- info: { adjacency := Std.HashMap.ofList [("a", [])] } -/
#guard_msgs in
#eval DiGraph.empty {} |>.addNode "a" none


/-- info: { adjacency := Std.HashMap.ofList [("a", ["b"])] } -/
#guard_msgs in
#eval DiGraph.empty {} |>.addNode "a" (some "b") 


/-- info: { adjacency := Std.HashMap.ofList [("a", [])] } -/
#guard_msgs in
#eval DiGraph.empty {} |>.addNodes "a" none


/-- info: { adjacency := Std.HashMap.ofList [("a", ["c", "b"])] } -/
#guard_msgs in
#eval DiGraph.empty {} |>.addNodes "a" (some ["b", "c"]) 


