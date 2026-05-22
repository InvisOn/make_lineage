import Std.Data.HashMap
import Std.Data.HashSet


open Std
open Option


structure DiGraph where
  adjacency : HashMap String (List String) := {}
  deriving Repr


namespace DiGraph


  def addTarget (graph : DiGraph) (target : String) (dep : Option String) : DiGraph :=
    match dep with
    | some dep => { 
        adjacency := graph.adjacency.getD target {} |> (dep :: ·) |> graph.adjacency.insert target
      }
    | none => {
        adjacency := graph.adjacency.getD target {} |> graph.adjacency.insert target 
      }


  def addTargets (graph : DiGraph) (target : String) (deps : Option (List String)) : DiGraph :=
    match deps with
      | some deps => deps.foldl (fun graph' dep => addTarget graph' target (some dep)) graph
      | none => graph.addTarget target none


  def reverseEdges (graph : DiGraph) : DiGraph :=
    reverse graph.adjacency.toList {}
  where
    reverse (adjacency : List (String × List String)) (acc : DiGraph) : DiGraph :=
      match adjacency with
        | (target, []) :: rest => acc.addTarget target none |> reverse rest
        | (target, deps) :: rest => deps.foldl (fun graph' dep => addTarget graph' dep (some target)) acc |> reverse rest
        | [] => acc


  def toString (graph : DiGraph) : String :=
      let entries := graph.adjacency.toList.mapTR fun (target, deps) =>
      s!"{target} -> [{", ".intercalate deps}]"
      "DiGraph {\n" ++ 
      "\n".intercalate entries ++ 
      "\n}"


  def toDotNodes (graph : DiGraph) : String :=
    createNodes graph.adjacency.toList []
  where
    createNodes (adjacency : List (String × List String)) (acc : List String) : String :=
      match adjacency with
        | [] => "\n".intercalate acc
        | (target, []) :: tail => createNode target none :: acc |> createNodes tail
        | (target, deps) :: tail => createNodes tail (deps.eraseDups.mapTR (fun dep => createNode target (some dep)) ++ acc)

    @[always_inline]
    createNode (target : String) (dep : Option String) : String :=
      match dep with
       | none => s!"  \"{target}\""
       | some dep => s!"  \"{target}\" -> \"{dep}\""


  /-- Will always termiante because a Makefile is always a DAG -/
  partial def getAllAncesters (node : String) (graph : DiGraph) : DiGraph :=
    findAllAncestors [node] {}
  where
    adjacency := graph.adjacency

    findAllAncestors (targets : List String) (depsGraph : DiGraph) : DiGraph :=
      match targets with
        | [] => depsGraph
        | target :: rest => findAllAncestors (rest ++ targets) (depsGraph.addTargets target adjacency[target]?)

  partial def getAllDescendents (graph : DiGraph) (target : String) : DiGraph :=
    graph.reverseEdges |>.getAllAncesters target


end DiGraph


instance : ToString DiGraph := ⟨DiGraph.toString⟩


def addDotHeader (dotNodes : String) (rankdir : String) (dir : String): String := s!"digraph G \{
  graph [rankdir={rankdir}]
  node [shape=box, style=solid, margin=\"0.3,0.1\"]
  edge [color=\"#00000088\", dir={dir}, penwidth=1.2, minlen=1]

{dotNodes}
}"


def parseMakeP (db : String) : DiGraph :=
  parseRules rules {}
where
  rules := db.splitOn "\n\n" |>.dropWhile (fun s => !s.endsWith "# Files") |>.drop 1

  parseRules (entries : List String) (acc : DiGraph) : DiGraph :=
    match entries with
      | [] => acc
      | head :: tail => 
        if head.startsWith "#" then
          parseRules tail acc
        else
          match head.splitOn "\n" |>.getD 0 "" |>.splitOn ":" with
            | ".PHONY" :: _ => parseRules tail acc
            | target :: deps :: [] => parseDeps deps |> acc.addTargets target |> parseRules tail
            | _ => parseRules tail acc

  @[always_inline]
  parseDeps (deps : String) : Option (List String) := 
    match deps with
    | "" => none
    | deps => some (deps.splitOn.dropWhile (fun (s : String) => s.length == 0))


-- TODO: implement only show ancesters and/or descendents of name
-- TODO: implement coloring by name
-- TODO: add unit tests
-- TODO: prove some stuff about the code maybe

