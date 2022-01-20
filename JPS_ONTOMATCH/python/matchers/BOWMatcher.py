import traceback
import sys
import pickle
import nltk
from gensim import *

#todo: multi:edit,jaro,qgram

from matchers.elementMatcher import *
import numpy
import math
class BOWMatcher(ElementMatcher):
    def __init__(self, mm):
        self.name = 'BOWMatcher'
        ElementMatcher.__init__(self, mm)

    #todo: cos similarity
    def prep(self):
        #build dict
        self.S.dictionary.merge_with( self.T.dictionary)
        dct = self.S.dictionary

        all = list(self.S.tokensDict.values()) + list(self.T.tokensDict.values())
        self.idGap = len(self.S.tokensDict.values())
        print(all)
        self.corpus = [dct.doc2bow(label) for label in all]


    def tuple2dct(self, tupleLs):
        return {a:b for a,b in tupleLs}

    def combineDct(self, da, db):
        for id, count in db.items():
            if id not in da.keys():
                da[id] = 0




    def dctItem2V(self,dctTokens1, dctTokens2):
        d1 = self.tuple2dct(dctTokens1)
        d2 = self. tuple2dct(dctTokens2)
        self.combineDct(d1, d2)
        self.combineDct(d2,d1)
        d1s = dict(sorted(d1.items()))
        d2s = dict(sorted(d2.items()))
        return list(d1s.values()), list(d2s.values())


    def cosSim(self, va, vb):
        return numpy.dot(va, vb) / (numpy.sqrt(numpy.dot(va,va)) * numpy.sqrt(numpy.dot(vb,vb)))

    def compare(self, id1, id2):
        e1 = self.S.tokensDict[id1]#
        e2 = self.T.tokensDict[id2]#
        vt1 = self.corpus[id1]#
        vt2 = self.corpus[id2+self.idGap]#
        v1, v2 = self.dctItem2V(vt1,vt2)
        #print('vector for '+str(e1) + str(v1))
        #print('vector for ' + str(e2) + str(v2))
        #print(str(self.cosSim(v1, v2)))

        re = self.cosSim(v1, v2)
        if math.isnan(re):
            return 0
        else:
            return re


if __name__ == '__main__':
    '''
    test compare method

    class testO():
        def __init__(self,d1,bd1):
            self.tokensDict = bd1
            self.dictionary = d1



    from gensim import *

    corpus1 = ['this', 'is', 'dog']
    corpus2 = ['is', 'cat']
    dict1,dict2 = corpora.Dictionary([corpus1]),corpora.Dictionary([corpus2])
    bd1 = {0: corpus1}
    bd2 = {0: corpus2}
    testS = (testO(dict1, bd1), testO(dict2,bd2))
    m = BOWMatcher(testS)
    m.compare(0,0)
    '''
    fileUrl = sys.argv[1]
    srcUrl = sys.argv[2]
    tgtUrl = sys.argv[3]
    match_method = sys.argv[4]

    try:
        srcPi = open(srcUrl, 'rb')
        srcOnto = pickle.load(srcPi)
        tgtPi = open(srcUrl, 'rb')
        tgtOnto = pickle.load(srcPi)
        matcher = BOWMatcher((srcOnto, tgtOnto))
        mm = getattr(matcher, match_method)
        # matchSerial for T, matchIndividual for I
        a = mm()
        a.render(fileUrl)
        print("success")
    except Exception:
        print(repr(Exception))
        print(traceback.print_exc())

