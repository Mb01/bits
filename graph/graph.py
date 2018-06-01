#/usr/bin/env python

class Graph(object):
    def __init__(self):
        self.connections = {}
    def connect(self, a, b):
        if a in self.connections:
            self.connections[a].append(b)
        else:
            self.connections[a] = [b]
    def reachable(self, a, b):
        ### find out if "b" can be reached from "a"
        def inner(a, b, visited):
            if a not in self.connections or a in visited:
                return False
            visited.append(a)
            if b in self.connections[a]:
                return True
            
            else:
                return any([inner(x, b, visited) for x in self.connections[a]])
        return inner(a, b, [])

class WeightedEdge(object):
    def __init__(self, v1, v2, weight):
        self.v1 = v1
        self.v2 = v2
        self.weight = weight

    def get_other(self, v):
        return self.v1 if v == self.v2 else self.v2
    

class Vertex(object):
    def __init__(self, name):
        self.name = name
        self.edges = []
        self.visited = False 
        self.distance = float('inf') # a bit of hack, though

class BiWeightedGraph(object): # foregoing inheritance
    # call for vertices by number
    def __init__(self):
        self.vertices = None

    def make_vertices(self, n):
        self.vertices = [Vertex(x) for x in range(n)]
        
    def connect(self, a, b, weight): #

        def dry(v, e):
            v.edges.append(e)
                
        edge = WeightedEdge(a,b,weight)
        dry(a, edge)
        dry(b, edge)

    def set_distances_to_infinity(self):
        for v in self.vertices:
            v.distance = float('inf')

    def set_visited_to_False(self):
        for v in self.vertices:
            v.visited = False

    def dijkstra(self, root):
        self.set_distances_to_infinity()
        self.set_visited_to_False()
        # get the actual objects and store as unvisited
        unvisited = list(self.vertices) # deep copy
        root.distance = 0 # root has no distance from self
        
        def get_min_distance(to_visit):
            return min(to_visit, key=lambda v: v.distance)

        def get_lightest_edge(adjacents):
                return min(adjacents, key=lambda e: e.weight)

        # this needs to redone, we can't include edges that have already
        # been used or we will get cycles setting weights back?
        # come to think of it that won't happen because least distance
        # 
        while unvisited:    
            visiting = get_min_distance(unvisited)
            unvisited.remove(visiting)
            edge = get_lightest_edge(visiting.edges)
            other = edge.get_other(visiting)
            other.distance = min(other.distance, visiting.distance + edge.weight)
        return other    
            
            


#################### TEST ####################
graph1 = Graph() # testing whether connectd
for x in ((1,2),(3,4),(4,5),(6,7),(6,8)):
    graph1.connect(x[0], x[1])

assert(graph1.reachable(1,4) == False)
assert(graph1.reachable(3,5) == True)
#################### END TEST ################

#################### TEST ####################
graph2 = BiWeightedGraph()
graph2.make_vertices(5)
vtxs = graph2.vertices
graph2.connect(vtxs[0],vtxs[1], 100)
graph2.connect(vtxs[0],vtxs[2], 200)
graph2.connect(vtxs[0],vtxs[3], 300)
graph2.connect(vtxs[1],vtxs[2], 100)

graph2.dijkstra(vtxs[0])

#################### END TEST ################


