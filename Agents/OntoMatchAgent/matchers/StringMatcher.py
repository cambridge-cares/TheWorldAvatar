#todo: multi:edit,jaro,qgram
from nltk.metrics.distance import jaro_similarity,edit_distance,jaro_winkler_similarity

from matchers.elementMatcher import *
from matchManager import *
class StringMatcher(ElementMatcher):
    def __init__(self, es):
        print('String Matcher')
        ElementMatcher.__init__(self, es)
        self.name ='StringMatcher'

    def compare(self, id1, id2):
        #print('compare:')
        #print(id1)
        #print(id2)
        e1 = self.S.entities[id1]
        e2 = self.T.entities[id2]
        #print(e1)
        #print(e2)
        return jaro_winkler_similarity(e1, e2)

    @classmethod
    def jaro_similarity(cls, a1, a2):
        return jaro_similarity(a1, a2)

    @classmethod
    def jaro_winkler_similarity(cls, a1, a2):
        return jaro_winkler_similarity(a1, a2)

    @classmethod
    def stringCompareEdit(cls, a1, a2):
        return edit_distance(a1, a2)/max(len(a1),len(a2))


if __name__ =="__main__":
    pass