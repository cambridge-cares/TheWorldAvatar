import nltk
from gensim import *

# penalize class unmatch

from matchers.elementMatcher import *


class Penalizer():
    def __init__(self, classAlign, icmap1,icmap2, p = 0.9, thre = 0.6):
        '''

        :param classAlign: classAlignment result, alignment obj
        :param icmap1: list of class/property for id
        :param icmap2: list of class/property for id
        :param p: penalizing score
        :param thre: threshold of class/property similarity score to determine if match
        '''
        self.classAlign = classAlign
        self.icmap1 = icmap1
        self.icmap2 = icmap2
        self.p = p
        self.thre = thre

    def sameClass(self, classlist1, classlist2):
        if classlist1 is None or classlist2 is None:
            return False
        for c1 in classlist1:
            for c2 in classlist2:
                vid = self.classAlign.search(c1, c2)#get matching score for property
                if vid is None:
                    continue
                else:
                    return True
        return False

    def penalPair(self, id1, id2):
        if not self.sameClass(self.icmap1[id1], self.icmap2[id2]):
            return 0

    def penal(self, instanceAlign):
        toupdate = []
        for id1,id2,value in instanceAlign.map:
            if not self.sameClass(self.icmap1[id1], self.icmap2[id2]):
                toupdate.append((id1, id2, value*self.p))
            else:
                toupdate.append((id1, id2, value))


        return Alignment(toupdate)
