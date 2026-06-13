import Cli
import Std
import Graph


open Cli


def runCommands (p : Parsed) : IO UInt32 := do
  let stdin <- IO.getStdin
  let input <- stdin.readToEnd

  let mut graph := parseMakeDatabase input

  if graph.isEmpty then
    IO.eprintln "Makefile database was empty or input was not a parsable Makefile database."
    return 1

  if let some node := p.flag? "prune-lineage" then
    match graph.getLineageNode node.value with
    | none => 
      IO.eprintln s!"Cannot prune the lineage of {node.value} because it was not found in the build graph."
      return 1
    | some lineage => graph := graph.pruneNodes lineage

  if let some node := p.flag? "keep-lineage" then
    match graph.getLineageNode node.value with
    | none => 
      IO.eprintln s!"Cannot keep only the lineage of {node.value} because it was not found in the build graph. Was this node pruned?"
      return 1
    | some lineage => graph := graph.getSubGraph lineage

  if let some node := p.flag? "highlight-lineage" then
    match graph.getLineageNode node.value with
    | none => 
      IO.eprintln s!"Cannot highlight the lineage of {node.value} because it was not found in the build graph. Was this node pruned?"
      return 1
    | some lineage => graph.toDot lineage |> IO.println 
      return 0

  IO.println graph.toDot

  return 0


def setupCommands : Cmd := `[Cli|
  make_lineage VIA runCommands; ["0.1.0"]
  "Parse Makefile database to dot"

  FLAGS:
    p, "prune-lineage"     : String; "Prune DAG of the given node's lineage. Pruning is done first."
    k, "keep-lineage"      : String; "Keep only the given node's lineage."
    l, "highlight-lineage" : String; "Highlight the given node's lineage."
]

