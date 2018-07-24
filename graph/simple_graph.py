#!/usr/bin/env python


class weighted_edge(object):
    def __init__(self, destination, weight):
        self.destination = destination
        self.weight = weight


class bidirectional_weight_graph(object):
    def __init__(self):
        self.connections = {}

    def add_edge(self, v1, v2, weight):
        def dry(a, b, weight):
            we = weighted_edge(b, weight)
            if a not in self.connections:
                self.connections[a] = [we]
            else:
                self.connections[a].append(we)
        dry(v1, v2, weight)
        dry(v2, v1, weight)


bwg = bidirectional_weight_graph()
# add edges from n to n+1 with weight 1 from 1 to 5
edges = zip(range(1, 5), range(2, 6), (1, ) * 4)
for e in edges:
    bwg.add_edge(*e)
# add a direct connection from 2 to 5 that should be be part of a min sp tree
bwg.add_edge(2, 5, 0)
