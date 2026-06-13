import Command


def main (args : List String) : IO UInt32 := do
  setupFlags.validate args

