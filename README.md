# make_visual

```
digraph G {
    graph [rankdir=RL]
    node [shape=box, style=solid, margin="0.3,0.1"]
    edge [color="#00000088", dir=back, penwidth=1.2, minlen=1]

    "Makefile"    [style="solid,filled"]
    "src/main.rs" [style="solid,filled"]
    "clean"

    "output.dot" -> "Makefile"
    "output.dot" -> "src/main.rs"
    "all" -> "output.pdf"
    "output.pdf" -> "output.dot"
}

```
