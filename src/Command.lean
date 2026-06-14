import Cli
import Std
import Graph

open Std

open Cli


def flagToHashSet (flag : Parsed.Flag) : HashSet String :=
  flag.as! (Array String) |> HashSet.ofArray


def runCommands (p : Parsed) : IO UInt32 := do
  let stdin <- IO.getStdin
  let input <- stdin.readToEnd

  let mut graph := parseMakeDatabase input

  if graph.isEmpty then
    IO.eprintln "Makefile database was empty or input was not a parsable Makefile database."
    return 1

  if let some nodes' := p.flag? "prune-lineage" then
    let lineage := flagToHashSet nodes' |> graph.getLineageNodes

    if !graph.contains lineage then
      IO.eprintln s!"Cannot prune the lineage of {nodes'.value} because it was not found in the build graph."
      return 1
    else
      graph := graph.pruneNodes lineage

  if let some nodes' := p.flag? "keep-lineage" then
    let lineage := flagToHashSet nodes' |> graph.getLineageNodes

    if !graph.contains lineage then
      IO.eprintln s!"Cannot keep only the lineage of {nodes'.value} because it was not found in the build graph. Was this node pruned?"
      return 1
    else
      graph := graph.getSubGraph lineage

  if let some nodes' := p.flag? "highlight-lineage" then
    let lineage := flagToHashSet nodes' |> graph.getLineageNodes

    if !graph.contains lineage then
      IO.eprintln s!"Cannot highlight the lineage of {nodes'.value} because it was not found in the build graph. Was this node pruned?"
      return 1
    else
      graph.toDot lineage |> IO.println 
      return 0

  IO.println graph.toDot

  return 0


def setupFlags : Cmd := `[Cli|
  make_lineage VIA runCommands; ["0.1.0"]
  "Parse Makefile database to dot"

  FLAGS:
    p, "prune-lineage"     : Array String; "Prune DAG of the given node's lineage. Pruning is done first. Example: -p nodeA,nodeB"
    k, "keep-lineage"      : Array String; "Keep only the given node's lineage. Example: -k nodeA,nodeB"
    l, "highlight-lineage" : Array String; "Highlight the given node's lineage. Example: -l nodeA,nodeB"
]

