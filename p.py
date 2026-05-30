# /// script
# requires-python = ">=3.11"
# dependencies = [
#   "networkx",
# ]
# ///

import networkx as nx
from functools import reduce
from typing import Optional

G = nx.DiGraph()
G.add_edges_from([("a", "b")])


print("=== DFS edges ===")
print(list(nx.dfs_edges(G, "a")))
print(list(nx.dfs_edges(G, "b")))


Graph = dict[str, list[str]]  # type alias for readability


def dfs_recursive(
    graph: Graph,
    node: str,
    visited: Optional[set[str]] = None,
) -> set[str]:
    if visited is None:
        visited = set()

    visited = visited | {node}
    # print(node, end=" ")  # process the node

    return reduce(
        lambda acc, neighbor: (
            dfs_recursive(graph, neighbor, acc) if neighbor not in acc else acc
        ),
        graph[node],
        visited,
    )


# Usage
graph: Graph = {
    "A": ["B"],
    "B": [],
}

print(dfs_recursive(graph, "A"))
# Output: A B D E F C
