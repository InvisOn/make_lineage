import Command
import Graph


def main (_args : List String) : IO UInt32 := do
  let makeP <- getStdin
  let rankdir := "RL"

  match parseMakeP makeP |>.toDotNodes with
    | "" => 
      IO.println "No viable graph"
      return 1
    | dotNodes =>
      addDotHeader dotNodes rankdir |> IO.println
      return 0
