import Std.Data.HashMap
import Std.Data.HashSet

open Std
open Option

-- set_option trace.compiler.ir.result true


structure DiGraph where
  adjacency : HashMap String (HashSet String) := {}
  deriving Repr


namespace DiGraph


def addEdge (g : DiGraph) (target : String) (dep : Option String) : DiGraph :=
  match dep with
  | some dep => { adjacency := g.adjacency.getD target {} |>.insert dep |> g.adjacency.insert target }
  | none => { adjacency := g.adjacency.getD target {} |> g.adjacency.insert target }


def addEdges (graph : DiGraph) (target : String) (deps : Option (List String)) : DiGraph :=
  match deps with
    | some deps => deps.foldl (fun g dep => addEdge g target (some dep)) graph
    | none => graph.addEdge target none


def toString (g : DiGraph) : String :=
    let entries := g.adjacency.toList.map fun (target, deps) =>
      s!"{target} -> [{", ".intercalate deps.toList}]"
    "DiGraph {\n" ++ "\n".intercalate entries ++ "\n}"


def to_dot (g : DiGraph) : String :=
  "digraph G {
  graph [rankdir=RL]
  node [shape=box, style=solid, margin=\"0.3,0.1\"]
  edge [color=\"#00000088\", dir=back, penwidth=1.2, minlen=1]\n\n" ++
  create_nodes g.adjacency.toList [] ++
  "}" 
where
  create_nodes (adjacency : List (String × HashSet String)) (acc : List String) : String :=
    match adjacency with
      | [] => String.intercalate "" acc
      | (target, deps) :: tail => 
        match deps.toList with
          | deps => match deps with
            | [] => create_node target none :: acc |> create_nodes tail
            | deps => create_nodes tail (deps.map (fun dep => create_node target (some dep)) ++ acc)

  @[always_inline]
  create_node (target : String) (dep : Option String) : String :=
    match dep with
     | none => s!"  \"{target}\"\n"
     | some dep => s!"  \"{target}\" -> \"{dep}\"\n"
    


end DiGraph


instance : ToString DiGraph := ⟨DiGraph.toString⟩


def parse_make_p (db : String) : Option DiGraph :=
  parse_db entries {}
where
  entries := (((db.splitOn "\n\n").dropWhile (fun s => !s.endsWith "# Files")).drop 1)

  parse_db (entries : List String) (acc : DiGraph) : Option DiGraph := do
    match entries with
      | [] => some acc
      | head :: tail => 
        if head.startsWith "#" then
          return (<- parse_db tail acc)
        else
          match (head.splitOn "\n").take 1 with
            | [""] => parse_db tail acc
            | [line] => match line.splitOn ":" with
              | target :: deps :: [] => parse_deps deps |> acc.addEdges target |> parse_db tail
              | _ => none
            | _ => none

  parse_deps (deps : String) : Option (List String) := 
    match deps with
    | "" => none
    | deps => some (deps.splitOn.dropWhile (fun (s : String) => s.length == 0))



-- TODO: implement filter by name and ancesters & descendents
-- TODO: implement print to dot & json
-- TODO: implement unit tests
-- TODO: implement prove some stuff about the code maybe

