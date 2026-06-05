import Std.Data.HashMap


open Std
open Option


structure DiGraph where
  empty ::
  adjacency : HashMap String (List String) := {}
  deriving Repr, BEq


namespace DiGraph


  def addEdge (graph : DiGraph) (nodeA : String) (predecessor : Option String) : DiGraph :=
    match predecessor with
    | some dep => { 
        adjacency := graph.adjacency.getD nodeA {} |> (dep :: ·) |>.mergeSort |> graph.adjacency.insert nodeA
      }
    | none => {
        adjacency := graph.adjacency.getD nodeA {} |>.mergeSort |> graph.adjacency.insert nodeA 
      }


  def addEdges (graph : DiGraph) (node : String) (predecessors : Option (List String)) : DiGraph :=
    match predecessors with
      | some descendents' => descendents'.foldl (fun graph' dep => addEdge graph' node (some dep)) graph
      | none => graph.addEdge node none


  def reverseEdges (graph : DiGraph) : DiGraph :=
    reverse graph.adjacency.toList {}
  where
    reverse (adjacency : List (String × List String)) (acc : DiGraph) : DiGraph :=
      match adjacency with
        | (node, []) :: rest => acc.addEdge node none |> reverse rest
        | (node, descendents) :: rest => descendents.foldl (fun graph' dep => addEdge graph' dep (some node)) acc |> reverse rest
        | [] => acc


  def toString (graph : DiGraph) : String :=
      let entries := graph.adjacency.toList.mapTR fun (node, descendents) =>
      s!"{node} -> [{", ".intercalate descendents}]"
      "DiGraph {\n" ++ 
      "\n".intercalate entries ++ 
      "\n}"


  def toDot (graph : DiGraph) (nodesToFill : HashSet String) : String :=
    s!"digraph G \{
  graph [rankdir=RL]
  node [shape=box, style=solid, margin=\"0.3,0.1\"]
  edge [color=\"#00000088\", dir=forward, penwidth=1.2, minlen=1]

{filledNodes}

{dotNodes}
}"
  where
    filledNodes := nodesToFill.toList.foldrTR (fun s acc => s!"\"{s}\" [style = \"solid,filled\"]\n" ++ acc) ""

    dotNodes := createNodes graph.adjacency.toList []

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

  
  def degree (graph : DiGraph) : Nat :=
    graph.adjacency.toList.foldrTR (fun n acc => n.snd.length + acc) 0


  def depthFirstSearch (graph : DiGraph) (source : String) : Option (HashSet String) :=
      if !graph.adjacency.contains source then
        none
      else
        search graph source {} graph.degree
    where
      search (graph : DiGraph) (node : String) (visited : HashSet String) (degree : Nat) : HashSet String :=
        -- Use the degree of the graph to make function total
        match degree with
         | 0 => visited.insert node
         | i + 1 => graph.adjacency.getD node [] |>.foldrTR (fun n acc => if !acc.contains n then search graph n acc i else acc) (visited.insert node)


  def findPredecessors (graph : DiGraph) (node : String) : Option (HashSet String) := do
      graph.reverseEdges.depthFirstSearch node |>.map (·.erase node)


  def findSuccessors (graph : DiGraph) (node : String) : Option (HashSet String) :=
    graph.depthFirstSearch node |>.map (·.erase node)


  def getLineageNode (graph : DiGraph) (node : String) : Option (HashSet String) :=
    if !graph.adjacency.contains node then
      none
    else
      match graph.findPredecessors node, graph.findSuccessors node with
        | none, none => some {node}
        | some ancestors, none => ancestors.insert node
        | none, some descendents => descendents.insert node
        | some ancestors, some descendents => HashSet.insertMany {node} ancestors |>.insertMany descendents


  def getSubGraph (graph : DiGraph) (nodesToKeep : HashSet String) : DiGraph :=
    { adjacency := graph.adjacency.filterMap predicateMap }
  where
    predicateMap (key : String) (value : List String) : Option (List String) :=
      if !nodesToKeep.contains key then
        none
      else
        value.filter (nodesToKeep.contains ·)


  def pruneLineage (graph : DiGraph) (nodesToPrune : HashSet String) : DiGraph :=
    { adjacency := graph.adjacency.filterMap predicateMap }
  where
    predicateMap (key : String) (value : List String) : Option (List String) :=
      if nodesToPrune.contains key then
        none
      else
        value.filter (!nodesToPrune.contains ·)


end DiGraph

instance : ToString DiGraph := ⟨DiGraph.toString⟩


def parseMakeP (db : String) : DiGraph :=
  parseRules rules {}
where
  rules := db.splitOn "\n\n" |>.dropWhile (!·.endsWith "# Files") |>.drop 1

  parseRules (rules : List String) (acc : DiGraph) : DiGraph :=
    match rules with
      | [] => acc
      | head :: tail =>
        if head.startsWith "#" then
          match head.splitOn "\n" with
            | "# Not a target:" :: _ :: "#  Builtin rule" :: _ | "# Not a target:" :: ".DEFAULT:" :: _ => 
              parseRules tail acc
            | "# Not a target:" :: dependency :: _ =>
              dependency.dropSuffix ":" |>.toString |> (acc.addEdge · none) |> parseRules tail
            | _ => parseRules tail acc
        else
          match head.splitOn "\n" |>.getD 0 "" |>.splitOn ":" with
            | ".PHONY" :: _ => parseRules tail acc
            | target :: deps :: [] => parseDeps deps |> acc.addEdges target |> parseRules tail
            | _ => parseRules tail acc

  @[always_inline]
  parseDeps (deps : String) : Option (List String) := 
    match deps with
    | "" => none
    | deps => some (deps.splitOn.dropWhile (·.length == 0))

