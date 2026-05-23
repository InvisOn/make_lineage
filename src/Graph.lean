import Std.Data.HashMap
import Std.Data.HashSet


open Std
open Option


structure DiGraph where
  adjacency : HashMap String (List String) := {}
  deriving Repr


namespace DiGraph


  def addNode (graph : DiGraph) (node : String) (dependency : Option String) : DiGraph :=
    match dependency with
    | some dep => { 
        adjacency := graph.adjacency.getD node {} |> (dep :: ·) |> graph.adjacency.insert node
      }
    | none => {
        adjacency := graph.adjacency.getD node {} |> graph.adjacency.insert node 
      }


  def addNodes (graph : DiGraph) (node : String) (descendents : Option (List String)) : DiGraph :=
    match descendents with
      | some descendents' => descendents'.foldl (fun graph' dep => addNode graph' node (some dep)) graph
      | none => graph.addNode node none


  def reverseEdges (graph : DiGraph) : DiGraph :=
    reverse graph.adjacency.toList {}
  where
    reverse (adjacency : List (String × List String)) (acc : DiGraph) : DiGraph :=
      match adjacency with
        | (node, []) :: rest => acc.addNode node none |> reverse rest
        | (node, descendents) :: rest => descendents.foldl (fun graph' dep => addNode graph' dep (some node)) acc |> reverse rest
        | [] => acc


  def toString (graph : DiGraph) : String :=
      let entries := graph.adjacency.toList.mapTR fun (node, descendents) =>
      s!"{node} -> [{", ".intercalate descendents}]"
      "DiGraph {\n" ++ 
      "\n".intercalate entries ++ 
      "\n}"


  def toDotNodes (graph : DiGraph) : String :=
    createNodes graph.adjacency.toList []
  where
    createNodes (adjacency : List (String × List String)) (acc : List String) : String :=
      match adjacency with
        | [] => "\n".intercalate acc
        | (node, []) :: tail => createNode node none :: acc |> createNodes tail
        | (node, descendents) :: tail => createNodes tail (descendents.eraseDups.mapTR (fun dep => createNode node (some dep)) ++ acc)

    @[always_inline]
    createNode (node : String) (dep : Option String) : String :=
      match dep with
       | none => s!"  \"{node}\""
       | some dep => s!"  \"{node}\" -> \"{dep}\""


  /-- Will always termiante because a Makefile is always a DAG -/
  partial def getAllAncesters (node : String) (graph : DiGraph) : Option DiGraph :=
    if adjacency.contains node then
      none
    else
      findAllAncestors [node] {}
  where
    adjacency := graph.adjacency

    findAllAncestors (queue : List String) (ancestors : DiGraph) : DiGraph :=
      match queue with
        | [] => ancestors
        | current :: rest => ancestors.addNodes current adjacency[current]? |> findAllAncestors (rest ++ queue) 

  partial def getAllDescendents (nude : String) (graph : DiGraph) : Option DiGraph :=
    graph.reverseEdges |>.getAllAncesters nude


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
            | target :: deps :: [] => parseDeps deps |> acc.addNodes target |> parseRules tail
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

