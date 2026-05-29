import Command
import Graph


def main (_args : List String) : IO UInt32 := do
  let makeP <- getStdin

  match parseMakeP makeP with
    | none => 
      IO.println "No viable graph"
      return 1
    | some graph =>
      graph.toDotNodes |> addDotHeader |> IO.println
      return 0

