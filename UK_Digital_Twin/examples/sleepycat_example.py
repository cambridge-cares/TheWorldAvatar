"""
A simple example showing how to use a Sleepycat store to do on-disk
persistence.
"""

from rdflib import Namespace, Literal, URIRef
from rdflib.graph import Graph, ConjunctiveGraph
from rdflib.store import NO_STORE, VALID_STORE
from rdflib.plugins.sleepycat import Sleepycat

from tempfile import mktemp

if __name__ == "__main__":
    path = mktemp()
    
    print('The temporary path is:', path)
    
    # Set up the store config
    store = Sleepycat()
    store.__open = True
    store.context_aware = True

    # Open previously created store, or create it if it doesn't exist yet  
    graph = ConjunctiveGraph(store = store, identifier = None)
    
    rt = graph.open(path, create=False)

    if rt == NO_STORE:
        # There is no underlying Sleepycat infrastructure, so create it
        graph.open(path, create=True)
    else:
        assert rt == VALID_STORE, "The underlying store is corrupt"

    print("Triples in graph before add: ", len(graph))

    # Now we'll add some triples to the graph & commit the changes
    rdflib = Namespace("http://rdflib.net/test/")
    graph.bind("test", "http://rdflib.net/test/")

    graph.add((rdflib["pic:1"], rdflib.name, Literal("Jane & Bob")))
    graph.add((rdflib["pic:2"], rdflib.name, Literal("Squirrel in Tree")))

    print("Triples in graph after add: ", len(graph))

    # display the graph in RDF/XML
    print(graph.serialize(format="xml").decode('utf-8'))

    # close when done, otherwise sleepycat will leak lock entries.
    graph.close()

    graph = None

    # reopen the graph

    graph = ConjunctiveGraph(store = store, identifier = None)

    graph.open(path, create=False)

    print("Triples still in graph: ", len(graph))

    graph.close()

    # Clean up the temp folder to remove the Sleepycat database files...
    import os

    for f in os.listdir(path):
        os.unlink(path + "/" + f)
    os.rmdir(path)