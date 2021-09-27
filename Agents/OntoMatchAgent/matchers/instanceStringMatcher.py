#todo: multi:edit,jaro,qgram
from nltk.metrics.distance import jaro_similarity

from matchers.elementMatcher import *
import matchers.StringMatcher as StringMatcher
from matchManager import *
class instanceStringMatcher(ElementMatcher):
    #add penalty for different class
    def __init__(self, es, pairIterator, penalizer,db=False):
        print('StringMatcher')
        ElementMatcher.__init__(self, es, pairIterator)
        self.name ='StringMatcher'
        self.penalizer = penalizer
        if db is True:
            self.con = self.connectDBnCreateTable(self.name)

    def compare(self, id1, id2):
        if self.penalizer.penalPair(id1, id2) == 0:
            return 0
        #print('sameClass '+self.S.instanceDict[id1]+' '+self.S.instanceDict[id2])

        e1 = self.S.individualNames[id1]
        e2 = self.T.individualNames[id2]
        #print(e1)
        #print(e2)
        return StringMatcher.jaro_winkler_similarity(e1, e2)






if __name__ =="__main__":
    pass