from operator import *
from rdflib import *
import sys
import json
from otsrdflib import OrderedTurtleSerializer

class Alignment(object):
    def __init__(self, arr = []):
        self.map =  self.sortById(arr)


    def add(self, mapping):
        self.map.append(mapping)


    def sortById(self, arr):
        s = sorted(arr, key=itemgetter(0, 1))
        return s


    def stableMarriage(self):
        #get a copy sorted idS, p
        #get a copy sorted idT, p
        #loop thru idS, go thru by p, propose
        def prefermtom1(w, m, m1):
            for mid, wid, p in wp:
                if wid == w:
                    if mid == m:
                        return True
                    if mid == m1:
                        return False
        mp = sorted(self.map, key=itemgetter(0,2), reverse=True)
        wp = sorted(self.map, key=itemgetter(1,2), reverse=True)
        mIds = set([a[0] for a in self.map])
        matched =  {k:False for k in mIds}
        matchW = dict()

        mpArr = {k:[] for k in mIds}
        for idS, idT, p in mp:
            mpArr[idS].append(idT)


        while (False in matched.values()):#or nothing left to match?
            #pick a free man
            freeM = None
            for id in mIds:
                if matched[id] == False:
                    freeM = id
                    break

            notFound = True
            for wid in mpArr[freeM]:

                #if w is free
                if wid not in matchW or matchW[wid] is None:
                    matchW[wid] = freeM
                    matched[freeM] = True
                    notFound = False
                    break


                else:
                    mNow = matchW[wid]
                    if prefermtom1(wid, freeM , mNow) is True:
                        matchW[wid] = freeM
                        matched[freeM] = True
                        matched[mNow] =False
                        notFound =False
                        break

            if notFound is True:
                matched[freeM] = True

        np = []
        for idS, idT, p in mp:
            if(idT, idS) in matchW.items():
                np.append((idS, idT, p))

        self.map = np



                    #no match





                    #if w is not free

    def search(self, id1, id2):
        for idx in range(0, len(self.map)):
            mid1, mid2, aP = self.map[idx]
            if id1 == mid1 and id2 == mid2:
                return idx
        return None


    #todo: add step
    def aggregate(self, a, step, mode = 'avg'):
        def aggregateAvg(a):

            assert (len(self.map) == 0 or len(self.map) == len(a.map))

            for idx,item in enumerate(a.map):
                oP = 0
                id1,id2,aP = a.map[idx]
                midx = self.search(id1, id2)
                nP = aP/step + oP *(1-1/step)
                self.map[midx] = (id1,id2,nP)



        def aggregateWeight(a):
            #
            assert (len(self.map) == 0 or len(self.map) == len(a.map))
            for idx,item in enumerate(a.map):
                oP = 0
                id1,id2,aP = a.map[idx]
                if idx < len(self.map):
                    oP = self.map[idx][2]
                    nP = aP * step + oP
                    self.map[idx] = (id1, id2, nP)
                else:

                    self.map.append((id1, id2, aP*step))


            pass
        if mode == 'avg':
            return aggregateAvg(a)
        elif mode == 'weight':
            return aggregateWeight(a)
        else:
            print('err, wrong aggregate mode')
            return None


    #replace ID with IRI to entity
    #TODO: check this funciton
    def id2Entity(self, onto1, onto2, entityListName = "entities"):
        pairList = []
        for id1, id2, p in self.map:
            #print("{} {}".format(id1,id2))
            srcE = getattr(onto1, entityListName)[id1]
            tgtE = getattr(onto2, entityListName)[id2]
            # srcType = self.srcOnto.types[id1]
            # tgtType = self.tgtOnto.types[id2]
            pairList.append([srcE, tgtE, p])
        self.map = pairList


    def filter(self, thre):
        #s = sorted(self.map, key=attrgetter(2), reverse=True)
        return Alignment([i for i in self.map if i[2]- thre>0])

    def filterDelete(self,thre):
        self.map = self.filter(thre).map

    def filterSByindex(self, sIdx):
        return Alignment([i for i in self.map if i[0]==sIdx])

    def update(self, upl):
        print('alignment before upate '+str(self.map))
        for tup in upl:
            for item in self.map:
                if item[0] == tup[0] and item[1] == tup[1] and tup[2] is not None:
                    self.map.append(tup)
                    self.map.remove(item)

        print('alignment after upate '+str(self.map))


    '''
    render an xml file
    '''
    def render(self, onto1IRI, onto2IRI,addr):
        g = Graph()
        A = Namespace("http://knowledgeweb.semanticweb.org/heterogeneity/alignment#")
        #add xml, level, type
        #add onto1, onto2
        g.bind('Alignment',A)
        #TODO: name vs location
        onto1Name= onto1IRI
        onto2Name = onto2IRI
        aNode = BNode()
        onto1 = URIRef(onto1Name)
        onto2 = URIRef(onto2Name)
        g.add((aNode, RDF.type, A.Alignment))
        g.add((aNode, A.onto1, onto1))
        g.add((aNode, A.onto2, onto2))
        g.add((onto1, RDF.type, A.ontology))
        g.add((onto1, A.location, Literal(onto1IRI)))
        g.add((onto2, RDF.type, A.ontology))
        g.add((onto2, A.location, Literal(onto2IRI)))
        #sorting
        self.map = sorted(self.map, key=itemgetter(0,1))
        count = 0
        for triple in self.map:
            #print(t3)
            count = count+1
            nodeName = addr+"#cell"+str(count)
            o = URIRef(nodeName)
            g.add((o, RDF.type, A.Cell))
            if onto1 in triple[0]:
                g.add((o, A.entity1, Literal(triple[0])))
                g.add((o, A.entity2, Literal(triple[1])))
            else:
                g.add((o, A.entity1, Literal(triple[0])))
                g.add((o, A.entity2, Literal(triple[1])))
            g.add((o, A.measure, Literal(triple[2], datatype=XSD.float)))
            g.add((o, A.relation, Literal('=')))
            g.add((aNode,A.map, o))
        #g.serialize(destination=addr, format='nt')
        out = open(addr, 'wb')
        serializer = OrderedTurtleSerializer(g)
        serializer.serialize(out)

    def renderJSON(self, onto1, onto2):
        ja = []
        #todo: confirm format is correct
        for triple in self.map:
            jo = {}
            jo['entity1'] = triple[0]
            jo['entity2'] = triple[1]
            jo['measure'] = triple[2]
            ja.append(jo)
        jostr = json.dumps(ja)

        return jostr






if __name__ == '__main__':
    #test rendering
    #a = Alignment([('cat','dog', 0.9)])
    #a.render('www.cat.com','www.dog.com', 'file:///D:/workwork/alignmennttest.owl')
    #aIRI, onto1, onto2, ja
    newIRI = sys.argv[1]
    onto1 = sys.argv[2]
    onto2 = sys.argv[3]
    jo = sys.argv[4]
    map = json.loads(jo)
    print('map: ')
    print(map)
    ma = Alignment(map)
    ma.render(onto1, onto2, newIRI)
