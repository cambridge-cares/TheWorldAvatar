from rdflib import Namespace, Literal, URIRef
from rdflib.graph import Dataset, Graph
from rdflib.plugins.memory import IOMemory

if __name__ == "__main__": 
    
    ns = Namespace("http://love.com#")
    id_ = URIRef("http://love.com/lovers/")

    mary = URIRef("http://love.com/lovers/mary")
    john = URIRef("http://love.com/lovers/john")

    cmary = URIRef("http://love.com/lovers/mary")
    cjohn = URIRef("http://love.com/lovers/john")
    
    #Set up store 
    store = IOMemory()   
    # store.__open = True
    # store.context_aware = True
    # store.graph_aware = True
    
    gmary = Graph(identifier=cmary)
    gmary.add((mary, ns["hasName"], Literal("Mary")))
    gmary.add((mary, ns["loves"], john))
    
    gjohn = Graph(identifier=cjohn)
    gjohn.add((john, ns["hasCuteName"], Literal("Johnny Boy")))
    
    
    
    # Create a new Dataset
    ds = Dataset(store = store)
    ds2 = Dataset(store = store)
    
    # # simple triples goes to default graph
    # ds.add((URIRef("http://example.org/a"),
    #     URIRef("http://www.example.org/b"),
    #     Literal("foo")))
    # # Create a graph in the dataset, if the graph name has already been
    # # used, the corresponding graph will be returned
    # # (ie, the Dataset keeps track of the constituent graphs)
    # g = ds.graph(URIRef("http://www.example.com/gr"))
    
    ds.add_graph(gmary)
    #ds2.add_graph(gjohn)
    
    
    # # add triples to the new graph as usual
    # g.add(
    #       (URIRef("http://example.org/x"),
    #       URIRef("http://example.org/y"),
    #       Literal("bar")) )
    # # alternatively: add a quad to the dataset -> goes to the graph
    # ds.add(
    #       (URIRef("http://example.org/x"),
    #       URIRef("http://example.org/z"),
    #       Literal("foo-bar"),g) )
    
    queryStr3 = """
    SELECT DISTINCT ?s
    WHERE 
        {
          graph <http://www.example.com/gr> {?s ?p ?o}
        }    
    """
    
    # qres = ds.query(queryStr3)
    
    # print('The query results are: ')
    # for n in qres:
    #     print(n)
    
    
    # querying triples return them all regardless of the graph
    # for t in ds.quads((None,None,None,None)):  # doctest: +SKIP
    #       print(t[3])  # doctest: +NORMALIZE_WHITESPACE

    for t in ds2.graphs():  # doctest: +SKIP
           print(t)  # doctest: +NORMALIZE_WHITESPACE

