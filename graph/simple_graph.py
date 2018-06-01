#!/usr/bin/env python

from itertools import starmap

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
edges = zip(range(1, 5), range(2, 6), (1, ) * 4)
for e in edges:
    bwg.add_edge(*e)
