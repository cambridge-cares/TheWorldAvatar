from ontomatch.matchers.elementMatcher import *
from ontomatch.modelTopic.modelTopic import topicModel
import operator
import numpy
import scipy
from scipy.spatial import distance
class DomainMatcher(ElementMatcher):
    def __init__(self, mm,addrs):
        self._addrModel = addrs[0]
        print(addrs[1])
        self._addrDict = addrs[1]
        self.name = 'domainMatcher'
        ElementMatcher.__init__(self, mm)


    def prep(self):
        '''
        prepration steps
        '''

        tm = topicModel(self._addrModel, addrDict=self._addrDict, mode='loadModel')
        self.TOPICNUM = len(tm.ldamodels.get_topics())
        #get document from target
        tm.printTopics()
        srcDoc = self.S.entitiesAsTxt()
        tgtDoc = self.T.entitiesAsTxt()
        #infer docs
        topicsIn1,wPos1,phil1 = tm.inferNewDoc([srcDoc])
        print('topics in 1 ' + str(topicsIn1))
        #remember the domain choice of each word in 1
        topicsIn2,wPos2,phil2 = tm.inferNewDoc([tgtDoc],0.000001)
        print('word topic list')

        self.topicsIds2 = topicIds2 = [k for (k, p) in topicsIn2]
        print('topics in 2 ' + str(topicsIn2))
        topicsInter = [b for b in topicsIn1 if b[0] in topicIds2]

        wordIds = set()
        for tpid, p in topicsInter:
            wordIds.update([id for id, p in tm.ldamodels.get_topic_terms(tpid)])
        self.interDomainWords  =  [tm.id2Word(wid) for wid in list(wordIds)]
        ######################################################################
        self.wordDict1 = {}  # {word token string : most Possible Topic id}record down most probable topic for each word
        self.wordDict2 = {}
        for wid, lst in phil1:
            lst.sort(key=operator.itemgetter(1),reverse=True)
            token =  tm.dictionary.id2token[wid]
            self.wordDict1[token] = lst
        print('++++++++++++++++++++++++++++++++++++++++++')
        for wid, lst in phil2:
            lst.sort(key=operator.itemgetter(1),reverse=True)
            token =  tm.dictionary.id2token[wid]
            self.wordDict2[token] = lst



        '''
        allWords = list(self.S.bowDict.values())+list(self.T.bowDict.values())
        for tokens in allWords:
            for word in tokens:

                if word not in tm.dictionary.token2id:
                    self.wordDict[word] = None
                if word not in self.wordDict:
                    wid = tm.dictionary.token2id[word]
                    tp = tm.ldamodels.get_term_topics(wid)
                    if len(tp)>0:
                        #todo: sort tp
                        tp.sort(key=operator.itemgetter(1), reverse=True)
                        print(tp)

                        self.wordDict[word] = tp[0][0]
                    else:
                        self.wordDict[word] = None
        '''

        #find union topic=>

    def normalizep(self, pTuple):
        pmax = max([t[1] for t in pTuple])
        return [(v, p/pmax) for v, p in pTuple]



    def compare(self, id1, id2):
        n1 = list(set(self.S.tokensDictLong[id1]))#
        n2 = list(set(self.T.tokensDictLong[id2]))

        #for each word, check domain
        '''
        if len(n1)== 0:
            print('err: zero len')
            print (self.S.entities[id1])
        # for each word, check domain
        if len(n2) == 0:
            print('err: zero len')
            print(self.T.entities[id2])
        '''



        #pIntersect,toa,tob,va,vb = self.domainScorePList(n1,n2)
        pIntersect,toa,tob,va,vb = self.getDomainScore2(n1,n2)

        if pIntersect  - 0 > 0.0000001:
        #if 'power' in n1:
            print('score for comparing:' + str(n1) + ' ' + str(n2) + ' ' +str(toa)+ ' '+str(tob)+ ' '+ str(pIntersect))
        return pIntersect




    def getDomainScore2(self,ta, tb):
        '''
        method2:
        for each word, get most probable topic, if same, return result
        :param tokens:
        :return:
        '''
        #todo: cosine similarity
        if len(ta) == 0 or len(tb)==0:
            return 0,None,None,None,None #todo: erase the zero length token issue
        def getTopic4Word(tokens, wordDict):
            t = []
            noneCount = 0
            for word in tokens:
                if word not in wordDict or len(wordDict[word]) is 0:
                    noneCount = noneCount+1
                    tw = None

                else:
                    tw = wordDict[word][0][0]#pick most likely topic only
                t.append(tw)
            return t,noneCount

        def getVec4TopicList(tl):
            v = numpy.zeros(self.TOPICNUM)
            for topicid in tl:
                if topicid is not None and topicid < self.TOPICNUM:
                    v[topicid] = v[topicid] +1
            return v
        toa,nNuma  = getTopic4Word(ta,self.wordDict1)
        tob, nNumb = getTopic4Word(tb,self.wordDict2)
        va = getVec4TopicList(toa)
        #if nNuma >0:
        #    nNuma = 1
        #if nNumb >0:
        #    nNumb = 1
        va = numpy.append(va, [1]*nNuma + [0]*nNumb) #append 0 to account for None
        vb = getVec4TopicList(tob)
        vb = numpy.append(vb, [0]*nNuma + [1]*nNumb)
        score = self.cosSim(va, vb)

        return (score,toa,tob,va,vb)

    def getDomainScore3(self,ta, tb):
        '''
        method2:
        for each word, get most probable topic, if same, return result
        :param tokens:
        :return:
        '''
        #todo: cosine similarity
        if len(ta) == 0 or len(tb)==0:
            return 0,None,None,None,None #todo: erase the zero length token issue
        def getTopic4Word(tokens, wordDict):
            t = []
            noneCount = 0
            for word in tokens:
                if word not in wordDict:
                    noneCount = noneCount+1
                    tw = None
                else:
                    tw = wordDict[word][0][0]#pick most likely topic only
                t.append(tw)
            return t,noneCount

        def getVec4TopicList(tl):
            v = numpy.zeros(self.TOPICNUM)
            for topicid in tl:
                if topicid is not None and topicid < self.TOPICNUM:
                    v[topicid] = v[topicid] +1
            return v
        toa,nNuma  = getTopic4Word(ta,self.wordDict1)
        tob, nNumb = getTopic4Word(tb,self.wordDict2)
        va = getVec4TopicList(toa)
        vb = getVec4TopicList(tob)
        sumA = 0
        sumB = 0
        for idx in range(self.TOPICNUM):
            if va[idx]!=0 and vb[idx]!=0:
                sumA = sumA + va[idx]
                sumB = sumB + vb[idx]

        score = max([sumA/len(toa), sumB/len(tob)])


        return (score,toa,tob,va,vb)


    def domainScorePList(self,ta,tb):
        #add each word to final distribution
        #use K-L dilligence

        def sumPModel(tokenList, d):
            allTp = []
            la = len(tokenList)
            for token in tokenList:
                noneCount = 0
                if token not in d or len(d[token]) is 0: #token not in dictionary
                    noneCount += 1
                else:
                    #normalize
                    topicIds, counts = list(zip(*d[token]))
                    allC = sum(counts)
                    normArr = [ (t,p/allC) for t,p in d[token]]
                    allTp.extend(normArr)

            tpMap = dict()
            for topicId,p in allTp:
                if topicId in tpMap:
                    tpMap[topicId] += p
                else:
                    tpMap[topicId] = p

            for topicId,p in tpMap.items():
                tpMap[topicId] = tpMap[topicId]/la

            #convert to vector
            v = numpy.zeros(self.TOPICNUM)
            for topicid,p in tpMap.items():
                v[topicid-1] = tpMap[topicid]

            return allTp,v

        toa,va = sumPModel(ta, self.wordDict1)

        tob,vb = sumPModel(tb, self.wordDict2)

        #todo:??????wrong!!!!!
        score = self.jensen_shannon_distance(va, vb)
        if numpy.isnan(score):
            score = 0


        return (score,toa,tob,va,vb)

    def jensen_shannon_distance(self,p, q):
        """
        method to compute the Jenson-Shannon Distance
        between two probability distributions
        """
        # calculate m
        m = (p + q) / 2

        # compute Jensen Shannon Divergence
        divergence = (scipy.stats.entropy(p, m) + scipy.stats.entropy(q, m)) / 2

        # compute the Jensen Shannon Distance
        #distance = numpy.sqrt(divergence)
        d = distance.jensenshannon(p,q,5)

        return 1 - d


    def cosSim(self, va, vb):
        return numpy.dot(va, vb) / (numpy.sqrt(numpy.dot(va,va)) * numpy.sqrt(numpy.dot(vb,vb)))

    def getDomainScoreJaccard(self, tokens):
        '''
        method1: considering each word => in intersection, score+1

        :param tokens:
        :return:
        '''
        score = 0
        for word in tokens:
            if word in self.interDomainWords:#
                score+=1

        return score/len(tokens)

        #scoring criteria


        #PASS

    def compareAncestor(self, id1, id2):

        if self.S.types[id1] == 'class' and self.T.types[id2] == 'class':
            print(id1)
            print(id2)
            an1 = self.S.classTree[id1]
            an2 = self.T.classTree[id2]
            e1 = self.S.entities[id1]
            e2 = self.T.entities[id2]
            if 'Thing' in an1:
                an1.remove('Thing')
            if 'Thing' in an2:
                an2.remove('Thing')
            t1,t2 = [],[]
            for clabel in an1:
                cid = self.S.name2id(clabel)
                if cid:
                    t1.extend(self.S.tokensDict[cid])
            for clabel in an2:
                cid = self.T.name2id(clabel)
                if cid:
                    t2.extend(self.T.tokensDict[cid])
            pIntersect,toa,tob,va,vb = self.getDomainScore2(t1,t2) # other four params are only for debugging
            #if pIntersect  - 0 > 0.00000001:
            print('score for comparing ancestor for:'+ str(e1) + ' ' + str(e2) + ' ' + str(an1) + ' ' + str(an2) + ' ' + str(pIntersect)+'  '+str(toa) + '  ' + str(tob))
            return pIntersect


    def compareClassByP(self, id1, id2):
        if self.S.types[id1] == 'class' and self.T.types[id2] == 'class':
            print(id1)
            print(id2)
        if id1 not in self.S.rangeMap or id2 not in self.T.rangeMap:
            return None
        an1 = self.S.rangeMap[id1]
        an2 = self.T.rangeMap[id2]
        e1 = self.S.entities[id1]
        e2 = self.T.entities[id2]
        if id1 in self.S.domainMap:
            an1.extend(self.S.domainMap[id1])
        if id2 in self.T.domainMap:
            an2.extend(self.T.domainMap[id2])
        #add
        t1, t2 = [], []

        for clabel in an1:
            cid = self.S.name2id(clabel)
            if cid:
                t1.extend(self.S.tokensDict[cid])
        for clabel in an2:
            cid = self.T.name2id(clabel)
            if cid:
                t2.extend(self.T.tokensDict[cid])
        pIntersect,toa,tob,va,vb = self.getDomainScore2(t1,t2)
        print('score for comparing property for:' + str(e1) + ' ' + str(e2) + ' ' + str(an1) + ' ' + str(an2) + ' ' + str(
                pIntersect) + '  ' + str(toa) + '  ' + str(tob))


        return pIntersect