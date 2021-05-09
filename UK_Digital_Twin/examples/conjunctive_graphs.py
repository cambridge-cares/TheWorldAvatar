"""
An RDFLib ConjunctiveGraph is an (unnamed) aggregation of all the named graphs
within a Store. The :meth:`~rdflib.graph.ConjunctiveGraph.get_context`
method can be used to get a particular named graph for use such as to add
triples to, or the default graph can be used
This example shows how to create named graphs and work with the
conjunction (union) of all the graphs.
"""

from rdflib import Namespace, Literal, URIRef
from rdflib.graph import Graph, ConjunctiveGraph
from rdflib.plugins.memory import IOMemory
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.namespace import NamespaceManager
from rdflib.store import NO_STORE, VALID_STORE

if __name__ == "__main__":

    ns = Namespace("http://love.com#")
    id_ = URIRef("http://love.com/lovers/")
    id2_ = URIRef("http://love.com/lovers2/")

    mary = URIRef("http://love.com/lovers/mary")
    john = URIRef("http://love.com/lovers/john")

    cmary = URIRef("http://love.com/lovers/mary")
    cjohn = URIRef("http://love.com/lovers/john")

    # store = IOMemory()
    store = Sleepycat()
    store.__open = True
    store.context_aware = True
    
    store2 = Sleepycat()
    store2.__open = True
    store2.context_aware = True


    g = ConjunctiveGraph(store=store, identifier = id_)
    # g2 = ConjunctiveGraph(store=store2, identifier = id2_)
    
    path1 = 'C:\\Users\\wx243\\Documents\\myRDFLibStore3'
    # path2 = 'C:\\Users\\wx243\\Documents\\myRDFLibStore4'
    sl1 = g.open(path1, create = False)
    # sl2 = g2.open(path2, create = False)
    
    if sl1 == NO_STORE:
    # There is no underlying Sleepycat infrastructure, so create it
        g.open(path1, create=True)
    # elif sl2 == NO_STORE:
    #     g2.open(path2, create=True)
    
    
    # namespace_manager_g = NamespaceManager(g)
    # namespace_manager_g.bind("love", ns)
    # all_ns = [n for n in g.namespace_manager.namespaces()]
    # print('The g is:')
    # print(g.serialize(format="pretty-xml").decode("utf-8"))
    # g.bind("love", ns)

    # add a graph for Mary's facts to the Conjunctive Graph with the same store
    gmary = Graph(store = store, identifier=cmary)
    # Mary's graph only contains the URI of the person she love, not his cute name
    gmary.add((mary, ns["hasName"], Literal("Mary")))
    gmary.add((mary, ns["loves"], john))

    # add a graph for John's facts to the Conjunctive Graph
    gjohn = Graph(store=store, identifier=cjohn)
    # John's graph contains his cute name
    gjohn.add((john, ns["hasCuteName"], Literal("Johnny Boy")))
    
    for q in g.quads((None,None,None,None)): 
        print(q[3])

    # g.add((mary, ns["hasName"], Literal("Mary")))   
    
    # tri = g.triples((None, None, None))

    # print('Tri is: ', next(tri))
    # print('Tri2 is: ', next(tri))
    # print('Tri3 is: ', next(tri))
    
    # for tri in g.triples((None, None, None)):
    #     print('tri is', tri)
    
    # print(gmary.get_context())
    # # context_ = Graph()
    
    # context_ = g.get_context(id_)
    
    # identifier_g = g.identifier#.n3()
    
    # spoc = g._spoc((mary, ns["hasName"], Literal("Mary"), g), default = True)
    
    # graph_name = set([graph.identifier for s, p, o, graph in g.quads((None, ns["hasName"], None ))])
    
    # print(g.__dict__)
    # print(gmary.__dict__)
    
    # print(len(graph_name))
    
    # print(g.store)
    
    # print(all_ns)
    
    # print(spoc)
    
    # print('The conjunctive graph is:')
    # print('g', g)
    # print('g2', g2)
    
    # print(g.__str__()) # the __str__ method will be automatically called in print() or str()
    
    # print(identifier_g)
    
    # print("========context===========")
    # print(context_.serialize(format="xml").decode("utf-8"))

    # # enumerate contexts
    # print('The context in the conjunctive graph 1 is: ')
    # for c in g.contexts():
    #     print("-- %s " % c)
    # print('The context in the conjunctive graph 2 is: ')
    # for c in g2.contexts():
    #     print("-- %s " % c)

    # # separate graphs
    
    # print("========gjohn===========")
    # print(gjohn.serialize(format="xml").decode("utf-8"))
    # print("=========gmary==========")
    # print(gmary.serialize(format="xml").decode("utf-8"))
    

    # # full graph
    # print("=========g==========")
    # print(g.serialize(format="xml").decode("utf-8"))
    # print("=========g2==========")
    # print(g2.serialize(format="xml").decode("utf-8"))

    # query the conjunction of all graphs
    
    queryStr1 = """
    PREFIX ns: <http://love.com#>
    PREFIX lovers: <http://love.com/lovers/>
    
    SELECT  ?g ?lover
    
    WHERE
    {
      GRAPH ?g {
    ?mary ns:loves ?lover .
    }
    }
    
    """
    queryStr2 = """
    PREFIX ns: <http://love.com#>
    SELECT DISTINCT  ?cuteName   
    
    WHERE   
         {
    ?mary ns:loves / ns:hasCuteName ?cuteName .
    }
    
    """
    
    queryStr3 = """
    SELECT DISTINCT ?g
    WHERE 
        {
          graph ?g {?s ?p ?o}
        }    
    """
    
    queryStr4 = """
    PREFIX ns: <http://love.com#>
    SELECT DISTINCT ?g ?g2 ?cuteName
    WHERE 
        {
          graph ?g {?mary ns:loves ?lover .}
          graph ?g2 {?lover ns:hasCuteName ?cuteName .}
        }    
    """
    
    queryStr5 = """
    PREFIX ns: <http://love.com#>
    PREFIX lovers: <http://love.com/lovers/>
    
    SELECT DISTINCT ?lover
    
    WHERE 
        {
          GRAPH lovers:mary {?mary ns:loves ?lover .}
        }    
    """ # do not need to specify the named graph via 'FROM' or 'FROM NAMED',
        # when query the conjunctive graph which contains the named graphs
        # the named graph can be simply identified by 'GRAPH' and the identifier of the named graph
    
    qres = g.query(queryStr5)
    
    # print('The query results are: ')
    # for n in qres:
    #     print(n)
    
    
    # xx = None
    # for x in g[mary : ns.loves/ ns.hasCuteName ]:
    #       xx = x
    # print("Q: Who does Mary love?")
    # print("A: Mary loves {}".format(xx)) 
   
    # xx = None
    # for x in gmary[mary : ns.loves ]:
    #     xx = x
    # print("Q: Who does Mary love?")
    # print("A: Mary loves {}".format(xx))

    # filepath =  "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\test\\Test_conjunctive_graph.owl\\"   
    # g.serialize(destination = filepath, format="application/rdf+xml")

    g.close()
    # g2.close()
    
    