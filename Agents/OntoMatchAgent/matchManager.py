import logging
import pickle
import sys
import time

import numpy
import pandas as pd

from alignment import Alignment
import blocking
import evaluate
import matchers
from matchers.GeoAttrFinder import GeoAttrFinder
from matchers.Penalizer import Penalizer
from matchers.MeronymRematcher import MeronymRematcher
import ontologyWrapper

class matchManager(object):
    def __init__(self, matchSteps, srconto, tgtonto, aggregation='average',thre=0.6, weight =None, paras=None, matchIndividuals=False, penalize=None, useAttrFinder=False):
        self.matchSteps = []
        #todo null handling
        self.matchSteps = matchSteps
        self.aggregation = aggregation
        self.thre= thre
        self.weight = weight
        self.paras = paras
        self.matchI = matchIndividuals
        self.penalize = penalize
        self.useAttrFinder = useAttrFinder
        self.srcOnto = srconto
        self.tgtOnto = tgtonto

    def runMatch(self, match_method = 'matchSerial', to1 = False, rematch = False, params_blocking=None):

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

        column_names = [ step for step in self.matchSteps]
        self.matchSteps = [getattr(matchers, step) for step in self.matchSteps]
        if match_method == 'matchWrite2Matrix':

            df_scores = None
            msize = (len(self.srcOnto.individualList), len(self.tgtOnto.individualList))
            resultMatrix = numpy.zeros(msize)
            for idx, matcherName in enumerate(self.matchSteps):
                mtime = time.time()
                pair_iterator = blocking.create_iterator(self.srcOnto, self.tgtOnto, params_blocking)
                logging.info('number of candidate matching pairs=%s', len(pair_iterator))

                if self.paras is not None and self.paras[idx] is not None:
                    matcher = matcherName((self.srcOnto, self.tgtOnto), pair_iterator, *self.paras[idx])
                else:
                    matcher = matcherName((self.srcOnto, self.tgtOnto), pair_iterator)
                mm = getattr(matcher, match_method)
                # mm_result is a numpy matrix with similarity scores between 0 and 1 calculated by the current matcher
                mm_result = mm()
                resultMatrix = resultMatrix + mm_result*self.weight[idx]
                column = column_names[idx]
                df_scores = self.add_similarity_scores_to_dataframe(resultMatrix, df_scores, column, self.srcOnto, self.tgtOnto, params_blocking, self.thre)
                mrunTime = time.time() - mtime
                logging.info('Finished matcher %s in %s', idx, mrunTime)

            #translate matrix to  alignment
            resultArr = []
            pair_iterator = blocking.create_iterator(self.srcOnto, self.tgtOnto, params_blocking)
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

            return self.A.map, df_scores


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

        return self.A.map, df_scores





    def showResult(self, a, entityListName,thre = 0, label = ''):
        #logging.debug('showing result')
        re = a.filter(thre)
        #re = re.stableMarriage()
        for id1,id2, p in re.map:
            srcE = getattr(self.srcOnto,entityListName)[id1]
            tgtE = getattr(self.tgtOnto,entityListName)[id2]
            logging.debug('%s src= %s, tgt= %s, score=%s', label, srcE, tgtE, p)



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


    def add_similarity_scores_to_dataframe(self, sim_matrix, df_scores, column_name, src_onto, tgt_onto, params_blocking, threshold):
        '''
        converts the sparse numpy matrix with row index pos1 and column index pos2 into a pandas dataframe
        with multi-index (idx_1, idx_2)
        '''
        rows = []
        pair_iterator = blocking.create_iterator(src_onto, tgt_onto, params_blocking)
        for pos1, pos2 in pair_iterator:
            score = sim_matrix[pos1, pos2]
            if score >= threshold:
                iri1 = src_onto.individualList[pos1]
                iri2 = tgt_onto.individualList[pos2]
                idx_1 = evaluate.getID(iri1)
                idx_2 = evaluate.getID(iri2)
                if df_scores is None:
                    rows.append({'idx_1': idx_1, 'idx_2': idx_2, column_name: score})
                else:
                    df_scores.at[(idx_1, idx_2), column_name] = score

        if df_scores is None:
            df_scores = pd.DataFrame(rows)
            df_scores.set_index(['idx_1', 'idx_2'], inplace=True)

        return df_scores

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
