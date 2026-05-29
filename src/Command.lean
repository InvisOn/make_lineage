
def getStdin : IO String := do
  let stdin <- IO.getStdin
  return (<- stdin.readToEnd)


structure Parser (a : Type) where
  unparsed : a
  parsed : List String
  deriving Repr


instance : Monad Parser where
  pure a := { unparsed := a, parsed := [] }
  bind ma f := 
    let mb := f ma.unparsed
    { unparsed := mb.unparsed, parsed := ma.parsed ++ mb.parsed}


-- TODO: add --reverse_edges -r
-- TODO: a good way to specify only including dependencies and/or ancestors of one or multiple nodes
-- TODO: add --dependencies -d
-- TODO: add --targets -t
-- TODO:  add --layout TB|BT|LR|RL

