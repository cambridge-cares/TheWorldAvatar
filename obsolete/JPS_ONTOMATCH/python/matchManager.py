import matchers
from alignment import Alignment
from matchers.Penalizer import Penalizer
from ontologyWrapper import Ontology

import pickle
class matchManager(object):
    def __init__(self, matchSteps, srcAddr, tgtAddr, aggregation = 'average',thre=0.6, weight =None, paras = None, matchIndividuals = False,penalize = None):
        self.matchSteps = []
        for step in matchSteps:
        #todo null handling
            self.matchSteps.append(getattr(matchers, step))
        self.srcAddr = srcAddr
        self.tgtAddr = tgtAddr
        self.aggregation = aggregation
        self.thre= thre
        self.weight = weight
        self.paras = paras
        self.matchI = matchIndividuals
        self.penalize = penalize
        self.mload()



    def mload(self):
        self.srcOnto = Ontology(self.srcAddr)
        self.tgtOnto = Ontology(self.tgtAddr)

        print('finish loading ontologies')



    def runMatch(self, match_method = 'matchSerial', to1 = False,filename = 'alignment.p'):
        #load ontology

        thisManager= self
        #run matching step by Step
        self.A = Alignment()

        if self.matchI:
            match_method = 'matchIndividuals'
            entityListName = 'individualList'
        else:
            entityListName = 'entities'

        for idx, matcherName in enumerate(self.matchSteps):
            if self.paras is not None and self.paras[idx] is not None:
                matcher = matcherName((self.srcOnto, self.tgtOnto),*self.paras[idx])
            else:
                matcher = matcherName((self.srcOnto, self.tgtOnto))
            mm = getattr(matcher, match_method)
            a = mm()
            self.A.aggregate(a, self.weight[idx], mode='weight')
            print('raw', str(idx))
            self.showResult(a,entityListName,0.3,label = 'raw'+ str(idx))
            #self.showResult(self.A,0.1,'step'+ str(idx))
            #print(self.A.map)
        #self.A.filterDelete(self.thre)

        print('after matching, filtering and stableMarriage')
        self.showResult(self.A,entityListName,0.4)

        if self.penalize is not None and 'class' in self.penalize:
            #penalize for non match class
            p = Penalizer(self.penalize['align'],self.srcOnto.icmap, self.tgtOnto.icmap)
            self.A = p.penal(self.A)
            self.showResult(self.A,entityListName)
        if self.penalize is not None and 'property' in self.penalize:
            #penalize for non match p
            p = Penalizer(self.penalize['align'],self.srcOnto.ipmap, self.tgtOnto.ipmap)
            self.A = p.penal(self.A)
            self.showResult(self.A,entityListName)

        if to1:
            self.A.stableMarriage()

        #self.showResult(self.A,entityListName,0.5)
        self.A.filterDelete(0.5)

        linkList= self.getConflictings(self.A)
        pairList= self.getMatchWithName(self.A,entityListName)

        #print(pairList)
        #print(linkList)
        #return [ list of entities pair , list of list of conflicting for that pair   ]
        self.save(pairList, filename)
        return [pairList, linkList]
        #<matchers.elementMatcher.ElementMatcher object at 0x222CBC70>
        #<__main__.ElementMatcher object at 0x02027330>



    def showResult(self, a, entityListName,thre = 0, label = ''):
        print("shwoing result")
        re = a.filter(thre)
        #re = re.stableMarriage()
        for id1,id2, p in re.map:
            srcE = getattr(self.srcOnto,entityListName)[id1]
            tgtE = getattr(self.tgtOnto,entityListName)[id2]
            #tgtType = self.tgtOnto.types[id2]
            print(label+' src entity:' +' '+ str(srcE)+' tgt entity:'+' '+str(tgtE)+"  "+str(p))



    def getMatchWithName(self,a,entityListName):

        pairList = []

        for id1, id2, p in a.map:

            srcE = getattr(self.srcOnto,entityListName)[id1]
            tgtE = getattr(self.tgtOnto,entityListName)[id2]
            #srcType = self.srcOnto.types[id1]
            #tgtType = self.tgtOnto.types[id2]
            pairList.append([srcE,tgtE,p])

        return pairList


    def getConflictings(self,a):

        conList = []
        for pair1id  in range(len(a.map)):
            conOfPair1 = []
            pair1= self.A.map[pair1id]
            for pair2id in range(len(a.map)):
                if pair1id is pair2id:
                    continue
                pair2 = self.A.map[pair2id]
                pair1S, pair1T,p = pair1
                pair2S, pair2T,p = pair2

                if pair1S == pair2S or pair1T == pair2T:#has conflict!
                    conOfPair1.append(pair2id)
            conList.append(conOfPair1)

        return conList

    def searchS(self,name):
        if name not in self.srcOnto['entityListName']:
            return 'not found'
        idx = self.srcOnto['entityListName'].index(name)
        a = self.A.filterSByindex(idx)
        self.showResult(a)


    def save(self, obj,name):
        #todo: save result as pkl so one can reload again
        pickle.dump(obj, open(name, "wb"))











if __name__ == '__main__':
    #test
    matchSteps = ['StringMatcher', 'BOWMatcher','DomainMatcher']
    w = [0.4, 0.4,0.2]
    #paras = [None, None, [("model/test/model30t30p2a.gensim","model/dictionary2.gensim"), 'compare']]
    #paras = [None, None, [("model/modellevel5/model50t30pautoa.gensim","model/modellevel5/dictlevel3full.gensim"), 'compare']]
    #paras = [None, None, [("model/modellevel2/model30t30pautoa.gensim", "model/modellevel2/dictionarylevel2.gensim"), 'compare']]
    #best for now
    paras = [None, None, [("model/modellevel2/model30t5p5a.gensim", "model/modellevel2/dictionarylevel2.gensim"), 'compare']]
    #paras = [None, None, [("model/modelRe/model20t30pautoa.gensim", "model/modelRe/dictionarylevel1.gensim"), 'compare']]
    #paras = [None, None, [("model/modelBase/baseClassmodel100t5pautoa.gensim", "model/modelBase/baseclass.gensim"), 'compare']]

    #paras = [None, None, [("model/modelRe/model50t2p2a.gensim", "model/modelRe/dictionarylevel1.gensim"), 'compare']]
    #"C:/Users/Shaocong/WORK/ontoMatchData/PowerPlant.owl"

    m = matchManager(matchSteps,"C:/Users/Shaocong/WORK/ontoMatchData/dbpedia_2014.owl", "C:/Users/Shaocong/WORK/ontoMatchData/ontology/PowerPlant.owl",thre = 0.6, weight = w, paras = paras)

    m.runMatch()

    #m.searchS('installedCapacity')
    #print(m.tgtOnto.entities[8])

