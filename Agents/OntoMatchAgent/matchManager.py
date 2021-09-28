import logging
import pickle
import sys
import time

import numpy

from alignment import Alignment
import blocking
import matchers
from matchers.GeoAttrFinder import GeoAttrFinder
from matchers.Penalizer import Penalizer
from matchers.MeronymRematcher import *
from ontologyWrapper import Ontology

class matchManager(object):
    def __init__(self, matchSteps, srcAddr, tgtAddr, aggregation = 'average',thre=0.6, weight =None, paras = None, matchIndividuals = False,penalize = None,useAttrFinder=False):
        self.matchSteps = []
        #todo null handling
        self.matchSteps = matchSteps
        self.srcAddr = srcAddr
        self.tgtAddr = tgtAddr
        self.aggregation = aggregation
        self.thre= thre
        self.weight = weight
        self.paras = paras
        self.matchI = matchIndividuals
        self.penalize = penalize
        self.useAttrFinder = useAttrFinder
        self.load()



    def load(self):

        if self.srcAddr.endswith('.pkl'):
            with open(self.srcAddr,'rb') as file:
                self.srcOnto = pickle.load(file)
        else:
            self.srcOnto = Ontology(self.srcAddr)
            #self.srcOnto._load(False)

        if self.tgtAddr.endswith('.pkl'):
            with open(self.tgtAddr,'rb') as file:
                self.tgtOnto = pickle.load(file)
        else:
            self.tgtOnto = Ontology(self.tgtAddr)
            #self.tgtOnto._load(False)
            #self.tgtOnto = Ontology(self.tgtAddr)

        logging.info('finish loading ontologies')

    def default_params(self):
        return {
            'blocking': {'name': 'FullPairIterator'}
        }

    def runMatch(self, match_method = 'matchSerial', to1 = False, rematch = False, params=None):

        if params is None:
            params = self.default_params()

        #load ontology

        #run matching step by Step
        self.A = Alignment()   

        if self.matchI:
            entityListName = 'individualList'
            if self.penalize is not None:
                p = Penalizer(self.penalize['align'],self.srcOnto.icmap, self.tgtOnto.icmap)
                for idx, matcher in enumerate(self.matchSteps):
                    self.paras[idx] = [p]
        else:
            entityListName = 'entities'

        #find extra attributes from names
        if self.useAttrFinder:
            a = GeoAttrFinder()
            extraS = a.findExtraGeoAttrSingleOnto(self.srcOnto)
            extraT = a.findExtraGeoAttrSingleOnto(self.tgtOnto)
            for idx, matcher in enumerate(self.matchSteps):
                if  matcher=="ValueMatcher":
                    self.paras[idx].append((extraS,extraT))

        self.matchSteps = [getattr(matchers, step) for step in self.matchSteps]
        if match_method is 'matchWrite2Matrix':

            msize = (len(self.srcOnto.individualList), len(self.tgtOnto.individualList))
            resultMatrix = numpy.zeros(msize)
            for idx, matcherName in enumerate(self.matchSteps):
                mtime = time.time()
                pair_iterator = blocking.create_iterator(self.srcOnto, self.tgtOnto, params['blocking'])

                if self.paras is not None and self.paras[idx] is not None:
                    matcher = matcherName((self.srcOnto, self.tgtOnto), pair_iterator, *self.paras[idx])
                else:
                    matcher = matcherName((self.srcOnto, self.tgtOnto), pair_iterator)
                mm = getattr(matcher, match_method)
                resultMatrix = resultMatrix + mm()*self.weight[idx]
                mrunTime = time.time() - mtime
                logging.info('Finished matcher %s in %s', idx, mrunTime)

            #translate matrix to  alignment
            resultArr = []
            pair_iterator = blocking.create_iterator(self.srcOnto, self.tgtOnto, params['blocking'])
            for idxS, idxT in pair_iterator:
                result = resultMatrix[idxS,idxT]
                if result >= self.thre:
                    resultArr.append((idxS, idxT, result))

            logging.info('number of pairs with scores larger than threshold=%s', len(resultArr))

            self.A = Alignment(resultArr)
            if to1:
                self.A.stableMarriage()
            if rematch is True:
                logging.info('rematch')
                self.A = MeronymRematcher((self.srcOnto, self.tgtOnto), self.A).rematch()
            return


        for idx, matcherName in enumerate(self.matchSteps):
            mtime = time.time()
            if self.paras is not None and self.paras[idx] is not None:
                matcher = matcherName((self.srcOnto, self.tgtOnto),*self.paras[idx])
            else:
                matcher = matcherName((self.srcOnto, self.tgtOnto))
            mm = getattr(matcher, match_method)
            a = mm()
            #self.showResult(a,entityListName,0.0,'step'+ str(idx))
            self.A.aggregate(a, self.weight[idx], mode='weight')
            logging.info('raw  %s', idx)
            mrunTime = time.time() - mtime
            logging.info('Finished matcher %s in %s', idx, mrunTime)


        '''
            #penalize for non match class
            p = Penalizer(self.penalize['align'],self.srcOnto.icmap, self.tgtOnto.icmap)
            self.A = p.penal(self.A)
            self.showResult(self.A,entityListName)
        if self.penalize is not None and 'property' in self.penalize:
            #penalize for non match p
            p = Penalizer(self.penalize['align'],self.srcOnto.ipmap, self.tgtOnto.ipmap)
            self.A = p.penal(self.A)
            self.showResult(self.A,entityListName)
        '''
        self.A.filterDelete(self.thre)

        if to1:
            self.A.stableMarriage()

        if rematch is True:
            logging.info('rematch')
            self.A = MeronymRematcher((self.srcOnto, self.tgtOnto), self.A).rematch()
        #TODO: save to second list





    def showResult(self, a, entityListName,thre = 0, label = ''):
        #logging.debug('showing result')
        re = a.filter(thre)
        #re = re.stableMarriage()
        for id1,id2, p in re.map:
            srcE = getattr(self.srcOnto,entityListName)[id1]
            tgtE = getattr(self.tgtOnto,entityListName)[id2]
            logging.debug(label + ' src= %s, tgt= %s, score=%s', srcE, tgtE, p)



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



    def renderResult(self,onto1Name,onto2Name, saveAddress, isI=True):
        if isI:
            entityName='individualList'
        else:
            entityName = 'entities'
        self.A.render(self.srcOnto, self.tgtOnto, onto1Name,onto2Name, saveAddress, entityName)


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

