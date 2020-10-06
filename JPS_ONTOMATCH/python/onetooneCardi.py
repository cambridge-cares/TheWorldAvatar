import sys
from rdflib import *
from alignment import *
from collections import OrderedDict


if __name__ == '__main__':
    #TODO: check this IO
    alignmentAddress = sys.argv[1]
    writeAddress = sys.argv[2]

    #build this into a custom map
    g = Graph()
    g.load(alignmentAddress)
    e1map = OrderedDict()
    e2map = OrderedDict()
    alignmentlist = []
    A = URIRef("http://knowledgeweb.semanticweb.org/heterogeneity/alignment#")
    #construct alignmentlist
    for s, p, o in g.triples((None, RDF.type, A.cell)):
        print("{} is a cell".format(s))
        e1 = g.value(s, A.entity1)
        e2 = g.value(s, A.entity2)
        score = g.value(s, A.measure)
        idx1 = e1map[e1] = len(e1map)
        idx2 = e2map[e2] = len(e2map)
        alignmentlist.append((idx1, idx2, score))

    aObj = Alignment(alignmentlist)
    #run the algorithm
    aObj.stableMarriage()
    #now change id in result to IRI
    #construct two entity list, sorted by
    mockonto1  = lambda: None
    mockonto2  = lambda: None
    mockonto1.entities = e1map.values()
    mockonto2.entities = e2map.values()
    aObj.id2Entity(mockonto1,mockonto2)
    jstr = aObj.renderJSON(mockonto1,mockonto2)
    print(jstr)
    #TODO:check here, it seems that you need to first extract IRI domain and path from the input tmp alignment