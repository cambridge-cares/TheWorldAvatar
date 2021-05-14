#todo: multi:edit,jaro,qgram
from nltk.metrics.distance import jaro_similarity

from matchers.elementMatcher import *
import matchers.StringMatcher as StringMatcher
from matchManager import *
class instanceStringMatcher(ElementMatcher):
    #add penalty for different class
    def __init__(self, es):
        print('String Matcher')
        ElementMatcher.__init__(self, es)
        self.name ='StringMatcher'

    def compare(self, id1, id2):
        #print('compare:')
        #print(id1)
        #print(id2)
        e1 = self.S.individualNames[id1]
        e2 = self.T.individualNames[id2]
        #print(e1)
        #print(e2)
        return StringMatcher.edit_similarity(e1, e2)






if __name__ =="__main__":
    pass