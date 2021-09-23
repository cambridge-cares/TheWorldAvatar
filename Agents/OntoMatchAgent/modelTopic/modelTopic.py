#pre-processing
import json
import nltk
from nltk.stem import WordNetLemmatizer, PorterStemmer
from gensim import corpora, models
from functools import lru_cache
import pickle
import ijson
import re
import time
import numpy as np
#todo: 


class topicModel(object):

    def __init__(self, addrCorpus, addrDict = None, addrFile = None, mode = 'save'):
        '''
        :param addr: addrFile/corpus | addrDict | addrModel
        :param mode: ['save', 'loadCorpus', 'loadModel']
        '''
        wnl = WordNetLemmatizer()
        self.lemmatize = wnl.lemmatize
        if mode == 'save':
            self.corpus = self.buildDictG(addrFile,addrDict,addrCorpus)
        elif mode == 'loadCorpus':
            self.loadDict(addrDict)
            self.loadCorpus(addrCorpus)
        elif mode == 'loadModel':
            self.loadDict(addrDict)
            self.loadModel(addrCorpus)
        else:
            print('no such mode')




    def readDoc(self,addr):
        '''
        process json(with stats) into doc array
        :param addr:
        :return:
        '''
         #eliminate duplicate
        #combine into documents    
        #todo: show stats


        with open(addr, 'r') as f:
            docDict= dict()
            try:
                batch = ijson.items(f,'item')
                for snip in batch:

                        print(snip)
                        if "oldid" in snip['url'] or ".jpg" in snip['url'].lower(): #ignore oldid
                            continue
                        else:
                            name = snip['url'].replace( "https://en.wikipedia.org","")
                            if name not in docDict.keys():
                                docDict[name] = set()
                            else:
                                docDict[name].add(snip['content'])
            except:
                pass
            print('documents num: '+ str(len(docDict.values())))
            #for k,v in docDict.items():
                #print(k)
                #print(len(v))
                #for i in v:
                    #print(i)
                #print('____________')
        return [' '.join(list(doc)) for doc in docDict.values() if len(doc) != 0]

    def readDoc2group(self,addr):
        '''
        process json(with stats) into doc array
        :param addr:
        :return:
        '''
         #eliminate duplicate
        #combine into documents
        #todo: show stats


        with open(addr, 'r') as f:
            docDict= dict()
            cateDict = dict()
            batch = ijson.items(f,'item')
            for snip in batch:
                print(snip)
                if "oldid" in snip['url'] or ".jpg" in snip['url'].lower(): #ignore oldid
                    continue
                else:
                    name = snip['url'].replace( "https://en.wikipedia.org","")
                    keywordCategory = snip['parent']
                    if keywordCategory not in docDict.keys():
                        cateDict[keywordCategory] = set()
                    else:
                        cateDict[keywordCategory].add(name)

                    if name not in docDict.keys():
                        docDict[name] = set()
                    else:
                        docDict[name].add(snip['content'])
            print('documents num: '+ str(len(docDict.values())))
            #for k,v in docDict.items():
                #print(k)
                #print(len(v))
                #for i in v:
                    #print(i)
                #print('____________')
        #divide into two groups
        group1,group2 = [],[]
        for list in cateDict.values():
            indexMax = int(len(list)/2)
            for name in range(0, indexMax):
                group1.append(' '.join(list(docDict[name])))
            for name in range(indexMax, len(list)-1):
                group2.append(' '.join(list(docDict[name])))


        return group1

    def processDocs(self,addr):
        docs = self.readDoc(addr)
        cateList = dict()
        print('num of raw documents: ' + str(len(docs)))
        idx = 0
        for doc in docs:
            print(str(idx))
            processed = self.processDoc(doc)
            if processed is not None and len(processed) >= 10:#after processing
                docs[idx] = processed
                idx = idx+1

        for i in range(idx, len(docs)):#update in place, remove all rest
            del docs[i]

        return docs


    def processDocG(self,addr):
        docs = self.readDoc(addr)
        print('num of raw documents: ' + str(len(docs)))
        idx = 0
        for doc in docs:
            print(str(idx))
            processed = self.processDoc(doc)
            if processed is not None and len(processed) >= 10:#todo: excluding too small files
                yield processed
                idx = idx+1



    def loadDict(self,addr):
        print(addr)
        self.dictionary = corpora.Dictionary.load(addr)
        print(self.dictionary)
        print('finished loading dictionary')


    def id2Word(self,id):

        return self.dictionary[int(id)]

    def loadCorpus(self, addr):
        self.corpus = pickle.load( open(addr, 'rb'))
        print('corpus size'+str(len(self.corpus)))
        #print(self.corpus)
        print('finished loading corpus and dictionary')

    def buildDict(self,addr,addrDict,addrCorpus):
        docs =self.processDocs(addr)
        print('finished processing documents')
        self.dictionary = dictionary = corpora.Dictionary(docs)
        self.corpus= [dictionary.doc2bow(doc) for doc in docs]
        print('doc len: '+ str(len(self.corpus)))
        pickle.dump(self.corpus, open(addrCorpus, 'wb'))
        dictionary.save(addrDict)
        print('finished building dictionary')

    def buildDictG(self,addr,addrDict,addrCorpus, updateDict = True):
        gene = self.processDocG(addr)
        self.dictionary = dictionary = corpora.Dictionary()
        corpus= []
        gene2 = self.processDocG(addr)


        if updateDict:
            while True:
                try:
                    doc = next(gene)
                    print('doc after:')
                    print(doc)
                    self.dictionary.add_documents([doc])
                except StopIteration:
                    print('end dict load')
                    break


            self.dictionary.filter_extremes(no_below=5, no_above=0.7, keep_n=5000, keep_tokens=['locat'])
            self.dictionary.save(addrDict)

        while True:
            try:
                doc = next(gene2)
                print(self.dictionary.doc2bow(doc))
                corpus.append(self.dictionary.doc2bow(doc))
            except StopIteration:
                print('end')
                break
        print('doc len: '+ str(len(corpus)))
        with open(addrCorpus, 'wb') as f:
            pickle.dump(corpus, f, protocol=4)
        print('finished building dictionary')
        return corpus




    def loadModel(self, addr):
        self.ldamodels = models.ldamodel.LdaModel.load(addr)

    # numPasses = the number of iterations of training.
    def LDA(self,numTopics, numPasses, numWords, alpha = "auto", numTopicsPrint = -1):
        print('now LDA model')
        if type(alpha) != str:
            print('alpha none string')
            alphaArr = []
            for i in range(0, numTopics):
                alphaArr.append(alpha)
            alphaArr = np.array(alphaArr)
            print(alpha)
        else:
            alphaArr = alpha
        self.ldamodels = models.ldamodel.LdaModel(self.corpus, num_topics =numTopics
            , id2word=self.dictionary, passes=numPasses,alpha=alphaArr, per_word_topics=True)
        self.ldamodels.save('../model/modelBase/baseClassmodel'+str(numTopics)+'t'+str(numPasses)+'p'+str(alpha)+'a.gensim')
        self.numTopics = numTopics
        #todo:nameConv
        self.printTopics(numTopicsPrint, numWords)



    def extendModel(self, addrNewDoc, addrNewCorpus,addDict = False, addrNewDict = ''):
        #update corpus and dict
        newCorpus = self.buildDictG(addrNewDoc,addrNewDict,addrNewCorpus, addDict)
        print(len(newCorpus))
        self.ldamodels.update(newCorpus)
        self.printTopics()


    def text2corpus(self, texts):
        return [self.dictionary.doc2bow(doc) for doc in texts]

    def inferNewDoc(self, texts,  p = 0.00000001):
        files = [self.processDoc(text) for text in texts]

        self.newCorpus = self.text2corpus(files)
        #print('converted to bow')
        #print(self.newCorpus)
        #self.ldamodels.update(self.newCorpus)
        #self.ldamodels.save('model'+str(self.numTopics)+'t'+str(self.numPasses)+'p'+name+'.gensim')
        #combine doc score
        newLda = []
        for idx in range(len(self.newCorpus)):
            bow = self.newCorpus[idx]
            newM = self.ldamodels.get_document_topics(self.newCorpus[idx],minimum_probability=p, per_word_topics =True,minimum_phi_value = 0.00000001)
            #print(newM[0])
            #print(newM[1])
            #print(newM[2])
            #for word, topiclist in newM[1]:
                #print(self.dictionary[word] + str(topiclist))
            #newLda.append(newM[0])
            return (newM[0],newM[1],newM[2])


    def getTopic(self, idx):
        return self.ldamodels.get_topic_terms(idx)

    def printTopics(self,  numTopicsPrint = -1, numWords = 20):
        print('print topicL:')
        topicModel._printTopics(self.ldamodels, numTopicsPrint = -1, numWords = 20 )
    @staticmethod
    def _printTopics(model, numTopicsPrint = -1, numWords = 20):

        topics = model.print_topics(num_topics=numTopicsPrint, num_words=numWords)
        for topic in topics:
            print(topic)



    def lemmatize_stemming(self, text):
        #print(text)
        return PorterStemmer().stem(self.lemmatize(text, pos='v'))
        #return WordNetLemmatizer().lemmatize(text, pos='v')

    def processDoc(self, text):
        start = time.time()
        print("start: " + str(start))
        #todo: extract cleaning?
        #one pass of all words in corpus
        print('processing doc')
        rawtokens =  nltk.word_tokenize(text)
        #print(rawtokens)
        tokens = []
        for rtoken in rawtokens:
            ptokens = topicModel.normalize(rtoken)
            if len(ptokens) != 0:
                tokens.extend(ptokens)
        sw = nltk.corpus.stopwords.words("english")
        sw.extend(["might","also","would", "first"])
        allwords = [ self.lemmatize_stemming(word) for word in tokens if len(word)>=3 and word not in sw ]
        print(allwords)
        print('num of words:'+str(len(allwords)))

        #print("end: " + str(time.time()-start) + " s")

        return allwords
    #todo
    def getIntersecTopics(self, docA, docB):
        pass
    @staticmethod
    def normalize(word):
        word = word.lower()
        realwords = re.findall(r"[a-z]+", word)
        return realwords

if __name__ == '__main__':
    #example: to load a json data source
    #tm = topicModel(addrCorpus='../model/modellevel2/corpuslevel2.pkl',addrDict='../model/modellevel2/dictionarylevel2.gensim', mode = 'loadCorpus')
    #or load a built dictionary and corpus
    #tm = topicModel(addrCorpus='../model/modellevel2/corpuslevel2.pkl',addrDict='../model/modellevel2/dictionarylevel2.gensim', mode = 'loadCorpus')

    #tm = topicModel('../model/modellevel5/corpusl3part1.pkl', addrDict='../model/modellevel5/modellevel5full.gensim',addrFile='C:/Users/Shaocong/WORK/ontoMatchData/crawlWiki/wikiJPSkeyslevel5.json', mode = 'save')
    #or read a model!
    #tm = topicModel('../model/modellevel5/corpusl3full.pkl', addrDict='../model/dictlevel3full.gensim', mode = 'loadCorpus')
    #tm = topicModel('../model/modellevel2/corpuslevel2.pkl', addrDict='../model/modellevel2/dictionarylevel2.gensim', mode = 'loadCorpus')
    #tm.LDA(50, 10, 20, 5)
    #tm.printTopics()
    #now infer a new document
    #tm = topicModel('../model/modellevel2/baseClassmodel20t10p5a.gensim', addrDict='../model/modellevel2/baseclass.gensim', mode = 'loadModel')
    #tm.printTopics()


    #with open('../powerplantclasses.txt', 'r') as f:
    #    file1 = f.read()

    #tm.inferNewDoc([file1])
    #with open('../dbpediaclasses.txt', 'r') as f:
    #    file2 = f.read()

    #tm.inferNewDoc([file2])
    tm = topicModel('../model/modelBase/baseclass.pkl', addrDict='../model/modelBase/baseclass.gensim', addrFile= 'C:/Users/Shaocong/WORK/ontoMatchData/crawlWiki/baseclassl2.json',mode = 'save')
    #tm = topicModel('../model/modelBase/baseclass.pkl', addrDict='../model/modelBase/baseclass.gensim', mode = 'loadCorpus')

    #a = tm.dictionary.doc2bow(['locat'])
    #print(a)
    tm.LDA(50, 10, 20, 4)
    tm.printTopics()
    #dictionary.doc2bow(doc)

