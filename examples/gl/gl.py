# import golib, gogl, sys
import golib, sys

#off = gogl.OFFFile()
off = golib.goOFFFile()
off.read (sys.argv[1])
adj = off.get_adjacency_lists()
lists = golib.goFixedArraygoListInt()
off.getAdjacencyLists(lists)

print adj
