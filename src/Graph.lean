import Std.Data.HashMap


open Std
open Option


structure DiGraph where
  empty ::
  adjacency : HashMap String (List String) := {}
  deriving Repr, BEq


namespace DiGraph


  def addRule (graph : DiGraph) (target : String) (prerequisites : Option (List String)) : DiGraph :=
    match prerequisites with
    | some prerequisites' => prerequisites'.foldrTR (aux target) graph
    | none => add graph target none
  where
    add (graph : DiGraph) (nodeA : String) (predecessor : Option String) : DiGraph :=
      match predecessor with
      | some dep => { 
          adjacency := graph.adjacency.getD nodeA {} |> (dep :: ·) |>.mergeSort |> graph.adjacency.insert nodeA
        }
      | none => {
          adjacency := graph.adjacency.getD nodeA {} |>.mergeSort |> graph.adjacency.insert nodeA 
        }

    @[always_inline]
    aux (target : String) (prerequisite : String) (graph : DiGraph) : DiGraph :=
      add graph target (some prerequisite)


  def reverseEdges (graph : DiGraph) : DiGraph :=
    reverse graph.adjacency.toList {}
  where
    reverse (adjacency : List (String × List String)) (acc : DiGraph) : DiGraph :=
      match adjacency with
      | (node, []) :: rest => acc.addRule node none |> reverse rest
      | (node, descendents) :: rest => descendents.foldrTR (aux node) acc |> reverse rest
      | [] => acc

    @[always_inline]
    aux (node : String) (descendent : String) (graph : DiGraph) : DiGraph :=
      graph.addRule descendent (some [node])


  def toString (graph : DiGraph) : String :=
    "DiGraph {\n" ++ nodes ++ "\n}"
  where
    nodes := graph.adjacency.toList.mapTR aux |> "\n".intercalate

    @[always_inline]
    aux (adjacent : (String × List String)) : String :=
      s!"{adjacent.fst} -> [{", ".intercalate adjacent.snd}]"


  def toDot (graph : DiGraph) (lineageHighlightNodes : HashSet String := {}) : String :=
    s!"digraph G \{
  graph [rankdir=RL]
  node [shape=box, style=solid, margin=\"0.3,0.1\"]
  edge [color=\"#00000088\", dir=back, penwidth=1, minlen=1]

{filledNodes}{dotNodes}
}"
  where
    filledNodes := 
      if lineageHighlightNodes.isEmpty then
        ""
      else
        lineageHighlightNodes.toList.mapTR aux |> "\n".intercalate |> (· ++ "\n\n")

    @[always_inline]
    aux (node : String) : String :=
      s!"  \"{node}\" [style = \"solid,filled\", fillcolor=\"#A8C4E0\"]"

    dotNodes := createNodes graph.adjacency.toList []

    createNodes (adjacency : List (String × List String)) (acc : List String) : String :=
      match adjacency with
      | [] => "\n".intercalate acc
      | (node, []) :: tail => s!"  \"{node}\"" :: acc |> createNodes tail
      | (node, successors) :: tail => createNodes tail (successors.eraseDups.mapTR (createEdge node) ++ acc)

    @[always_inline]
    createEdge (nodeA : String) (nodeB : String) : String :=
      if lineageHighlightNodes.contains nodeA || lineageHighlightNodes.contains nodeB then
        s!"  \"{nodeA}\" -> \"{nodeB}\" [color=\"#5B8DB8\", penwidth=1]"
      else
        s!"  \"{nodeA}\" -> \"{nodeB}\""

  
  def degree (graph : DiGraph) : Nat :=
    graph.adjacency.toList.foldrTR (·.snd.length + ·) 0


  def isEmpty (graph : DiGraph) : Bool :=
    graph.degree == 0


  def contains (graph : DiGraph) (nodes : HashSet String) : Bool :=
    nodes.all (graph.adjacency.contains ·)


  def depthFirstSearch (graph : DiGraph) (source : String) : HashSet String :=
        search graph source {} graph.degree
    where
      search (graph : DiGraph) (node : String) (visited : HashSet String) (degree : Nat) : HashSet String :=
        -- Without structural recursion the the graphs degree this function would be partial.
        match degree with
        | 0 => visited.insert node
        | i + 1 => graph.adjacency.getD node [] |>.foldrTR (aux i) (visited.insert node)

      @[always_inline]
      aux (degree : Nat) (node : String) (visited : HashSet String) : HashSet String :=
         if !visited.contains node then search graph node visited degree else visited


  def findPredecessors (graph : DiGraph) (node : String) : HashSet String :=
      graph.reverseEdges.depthFirstSearch node |>.erase node


  def findSuccessors (graph : DiGraph) (node : String) : HashSet String :=
    graph.depthFirstSearch node |>.erase node


  def getLineageNode (graph : DiGraph) (node : String) : HashSet String :=
    graph.findPredecessors node |>.insertMany (graph.findSuccessors node) |>.insert node


  def getLineageNodes (graph : DiGraph) (nodes : HashSet String) : HashSet String :=
    nodes.fold (fun acc n => graph.getLineageNode n |> acc.insertMany) {}


  def getSubGraph (graph : DiGraph) (nodesToKeep : HashSet String) : DiGraph :=
    { adjacency := graph.adjacency.filterMap predicateMap }
  where
    predicateMap (key : String) (value : List String) : Option (List String) :=
      if !nodesToKeep.contains key then
        none
      else
        value.filter (nodesToKeep.contains ·)


  def pruneNodes (graph : DiGraph) (nodesToPrune : HashSet String) : DiGraph :=
    { adjacency := graph.adjacency.filterMap predicateMap }
  where
    predicateMap (key : String) (value : List String) : Option (List String) :=
      if nodesToPrune.contains key then
        none
      else
        value.filter (!nodesToPrune.contains ·)


end DiGraph


instance : ToString DiGraph := ⟨DiGraph.toString⟩


def parseMakeDatabase (database : String) : DiGraph :=
  addTargets rules {} |> addPrerequisites
where
  rules := database.splitOn "\n\n" |>.dropWhile (!·.endsWith "# Files") |>.drop 1

  addTargets (rules : List String) (acc : DiGraph) : DiGraph :=
    match rules with
    | [] => acc
    | head :: tail =>
      if head.startsWith "#" then
        addTargets tail acc
      else
        match head.splitOn "\n" |>.getD 0 "" |>.splitOn ":" with
        | ".PHONY" :: _ => addTargets tail acc
        | target :: prerequisites :: [] => parsePrerequisites prerequisites |> acc.addRule target |> addTargets tail
        | _ => addTargets tail acc

  @[always_inline]
  parsePrerequisites (deps : String) : Option (List String) := 
    match deps with
    | "" => none
    | deps => some (deps.splitOn.dropWhile (·.length == 0))

  getPrerequisites (graph : DiGraph) : HashSet String :=
    graph.adjacency.values.foldrTR (HashSet.ofList · |> ·.insertMany) {}

  addPrerequisites (graph : DiGraph) : DiGraph := 
    let unAddedPrerequisites := graph.adjacency.keys |> HashSet.ofList |> (getPrerequisites graph).diff 
    unAddedPrerequisites.toList.foldrTR aux graph

  @[always_inline]
  aux (node : String) (graph : DiGraph) : DiGraph :=
    if !graph.adjacency.keys.contains node then
      graph.addRule node none 
    else 
      graph

