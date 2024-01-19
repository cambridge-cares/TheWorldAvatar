from ontomatch.matchers.elementMatcher import *
from ontomatch.matchers.StringMatcher  import *
from ontomatch.matchers.BOWMatcher import BOWMatcher
import rdflib
from unicodedata import *
import re
import math
class ValueMatcher(ElementMatcher):
    def __init__(self, es, pairIterator, penalizer, extraMap=None):
        print('ValueMatcher')
        ElementMatcher.__init__(self, es, pairIterator)
        self.name = 'ValueMatcher'
        self.bowM = BOWMatcher(es, pairIterator)
        self.penalizer =penalizer
        self.extraValueMapS = None
        self.extraValueMapT = None
        if extraMap is not None:
            self.extraValueMapS,self.extraValueMapT = extraMap
        print('set up')


    def compare(self, id1, id2):
        '''
        overwrite compare, comapare by lists of properties on value map
        :param id1:
        :param id2:
        :return:
        '''
        #print('compare:')
        #print(id1)
        #print(id2)

        if self.penalizer.penalPair(id1, id2) == 0:
            return 0

        listS = self.S.valueMap.map[id1].copy()
        listT = self.T.valueMap.map[id2].copy()
        cmapS = self.S.icmap[id1].copy()
        cmapT = self.T.icmap[id2].copy()

        if 'NamedIndividual' in cmapS:
            cmapS.remove('NamedIndividual')
        if 'NamedIndividual' in cmapT:
            cmapT.remove('NamedIndividual')
        if cmapS is not None and len(cmapS) != 0:
            subclassS = [('type', rdflib.term.Literal(v, datatype=rdflib.namespace.XSD.string)) for v in cmapS]
            listS.extend(subclassS)
        if cmapT is not None and len(cmapT) != 0:
            subclassT = [('type', rdflib.term.Literal(v, datatype=rdflib.namespace.XSD.string)) for v in cmapT]
            listT.extend(subclassT)
        if self.extraValueMapS is not None and self.extraValueMapS[id1] is not None:
            extraS = self.extraValueMapS[id1].copy()
            listS.extend(extraS)
        if self.extraValueMapT is not None and self.extraValueMapT[id2] is not None:
            extraT = self.extraValueMapT[id2].copy()
            listT.extend(extraT)

        scores = self.findAllMatch(listS, listT)


        aggrescore=  self.aggregateForInstance_tversky(scores, len(listS),len(listT) )
        #print('matching scores:',scores )
        #print('aggregated score: ',aggrescore)
        return  aggrescore


    def findAllMatch(self, list1, list2):
        #print(list1)
        #print(list2)
        scores = [0] * len(list1)
        matched = [None] * len(list1)
        for idx1, item1 in enumerate(list1):
            for idx2, item2 in enumerate(list2):
                if idx2 in matched:
                    continue  # Already Taken, find elsewhere
                p1, v1 = item1
                p2, v2 = item2
                score = self.criteria(v1, v2,p1)
                if score != 0:
                    if matched[idx1] is None:  # No previous match for idx1
                        matched[idx1] = idx2
                        scores[idx1] = score
                    else:
                        oldscore = scores[idx1]
                        if score > oldscore:
                            scores[idx1] = score
                            matched[idx1] = idx2

        def NoneZero(x):
            if x == 0:
                return False
            else:
                return True
        return list(filter(NoneZero,scores))





    def aggregateForInstance(self,scores, lenSp, lenTp, wfactor = 0.2):
        return sum(scores) / (len(scores) + wfactor *(lenSp+lenTp - 2* len(scores)))

    def aggregateForInstance_tversky(self,scores, lenSp, lenTp, wfactorT = 0.6,wfactorS=0.6):
        return sum(scores) / (len(scores) + wfactorS *(lenSp - len(scores))+wfactorT *(lenTp - len(scores)))


    def comparePString(self, p1, p2):
        l1,l2 = len(p1),len(p2)
        ll,sl = max(l1,l2),min(l1,l2)
        if sl==0 or ll==0 or ll/sl > 3:
            return 0  #heuristic, eliminate if string length difference too large

        return StringMatcher.jaro_winkler_similarity(p1, p2)


    def compareDate(self, ds1,ds2):
        d1,d2 = self.matchDate(ds1),self.matchDate(ds2)
        #match year
        score = 0
        if d1['year'] == d2['year'] and d1['year'] is not None:
            score += 0.7
        if d1['month'] == d2['month'] and d1['month'] is not None:
            score += 0.2
        if d1['day'] == d2['day'] and d1['day'] is not None:
            score += 0.1

        return score
    def criteria(self, value1, value2, p=None):
        #rdflib.term.Literal('Distillate_Oil', datatype=rdflib.term.URIRef('http://www.w3.org/2001/XMLSchema#string'))
        #print('value1 ',value1.value)
        #print('value2 ', value2.value)
        stringSimThre = 0.6
        MarginValue = 0.1

        '''
        if p is not None and 'capacity' in p.toPython().lower():#apply relative margin to physical value#TODO: test unit instead
            v1,v2 = float(value1.value),float(value2.value)
            l = max(abs(v1), abs(v2))
            if v1 == 0 and v2 == 0 :
                return 1
            elif l == 0:
                print('werid value ', v1,v2)
                return 0
            else:
                #margin = abs(v1-v2)
                margin = abs(v1-v2)/l
                if margin <= MarginValue:
                    return 1
                else:
                    return 0
        '''
        if self.isNumberType(value1) and self.isNumberType(value2) : #else, use absolute margin
            v1,v2 = float(value1.value),float(value2.value)
            if v1 == 0 and v2 == 0 :
                return 1
            else:
                margin = abs(v1-v2)
                if margin <= MarginValue:
                    return 1
                else:
                    return 0

        if self.isDateType(value1) and self.isDateType(value2):
            score = self.compareDate(value1.value,value2.value)
            return score

        elif not self.isNumberType(value1) and not self.isNumberType(value2) and self.isStringType(value1)  and self.isStringType(value2):
            #compare string similarity
            #print('compare string')
            score = self.comparePString(value1.value,value2.value)
            if score <= stringSimThre:
                return 0
            return score

        else:
            #print('type mismatch')
            return 0

        #print('score ',score)




    def numberTypeMismatch(self, v1, v2):
        floatTypes = [ 'http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#decimal','http://www.w3.org/2001/XMLSchema#double']
        integerTypes = ['http://www.w3.org/2001/XMLSchema#integer']
        if v1.datatype in floatTypes and v2.datatype in integerTypes:
            return False
        elif v2.datatype in floatTypes and v1.datatype in integerTypes:
            return False
        else:
            return True

    def isNumberType(self,v):
        numberTypes = ['http://www.w3.org/2001/XMLSchema#float', 'http://www.w3.org/2001/XMLSchema#decimal',
                     'http://www.w3.org/2001/XMLSchema#double','http://www.w3.org/2001/XMLSchema#integer']
        if v.datatype == 'http://www.w3.org/2001/XMLSchema#integer' or v.datatype == 'http://www.w3.org/2001/XMLSchema#decimal' or  v.datatype == 'http://www.w3.org/2001/XMLSchema#float' or  v.datatype == 'http://www.w3.org/2001/XMLSchema#double':
            return True
        elif isinstance(v.value, float) or isinstance(v.value, int):
            return True
        elif isinstance(v.value, str):
            try:
                d = float(v.value)
                return True
            except ValueError:
                return False


    def matchDate(self,s):
        dateRegex1 = '^(0?[1-9]|1[012])?[- \/]?(0?[1-9]|[12][0-9]|3[01])?[- \/]?([12]\d{3})?$'
        dateRegex2 = '^([12]\d{3})?[-\/]?(0?[1-9]|1[012])?[- \/.]?(0?[1-9]|[12][0-9]|3[01])?$'
        s = str(s)
        a = re.match(dateRegex2,s)
        if a is None:
            b = re.match(dateRegex1,s)
            if b is None:
                return None
            if b[1] is None and b[2] is None and b[3] is None:
                return None
            else:
                return {'day':b[2],'month':b[1],'year':b[3]}
        else:
            return {'day':a[3],'month':a[2],'year':a[1]}



    def isDateType(self,v):

        return v.datatype == 'http://www.w3.org/2001/XMLSchema#date' or self.matchDate(v.value) is not None
    def isStringType(self,v):
        return v.datatype == 'http://www.w3.org/2001/XMLSchema#string' or isinstance(v.value,str)


if __name__ =="__main__":
    pass