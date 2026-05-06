import Graph


def main : IO Unit := do
  let stdin <- IO.getStdin
  let input <- stdin.readToEnd

  match parse_make_p input with
  | none => IO.println "none"
  | some g => IO.println g.to_dot

