import sys
from rdflib import *
from alignment import *
from collections import OrderedDict


if __name__ == '__main__':
    #TODO: check this IO
    alignmentAddress = sys.argv[1]

    #build this into a custom map
    g = Graph()
    g.load(alignmentAddress)
    e1map = OrderedDict()
    e2map = OrderedDict()
    alignmentlist = []
    A = Namespace("http://knowledgeweb.semanticweb.org/heterogeneity/alignment#")
    #construct alignmentlist
    for s, p, o in g.triples((None, RDF.type, A.Cell)):
        e1name = g.value(s, A.entity1)
        e2name = g.value(s, A.entity2)
        score = float(g.value(s, A.measure).toPython())
        if e1name in e1map:
            idx1 = e1map[e1name]
        else:
            idx1 = e1map[e1name] = len(list(e1map.keys()))
        if e2name in e2map:
            idx2 = e2map[e2name]
        else:
            idx2 = e2map[e2name] = len(list(e2map.keys()))
        #print("{} is a cell with e1{} e2{} score{}".format(s,e1name,e2name,score))
        alignmentlist.append((idx1, idx2, score))

    aObj = Alignment(alignmentlist)
    #run the algorithm
    aObj.stableMarriage()
    #now change id in result to IRI
    #construct two entity list, sorted by
    mockonto1  = lambda: None
    mockonto2  = lambda: None
    mockonto1.entities = list(e1map.keys())
    mockonto2.entities = list(e2map.keys())
    aObj.id2Entity(mockonto1,mockonto2)
    jstr = aObj.renderJSON(mockonto1,mockonto2)
    print(jstr)
    #TODO:check here, it seems that you need to first extract IRI domain and path from the input tmp alignment