from matchers.elementMatcher import *
from matchers.StringMatcher  import *
from matchers.BOWMatcher import BOWMatcher
import rdflib
from unicodedata import *
import re
import math
class ValueMatcher(ElementMatcher):
    def __init__(self, es):
        print('Value Matcher')
        ElementMatcher.__init__(self, es)
        self.name ='ValueMatcher'

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


        listS = self.S.valueMap.map[id1]
        listT = self.T.valueMap.map[id2]
        scores = self.findAllMatch(listS, listT)

        aggrescore=  self.aggregateForInstance(scores, len(listS),len(listT) )
        #print('matching scores:',scores )
        #print('aggregated score: ',aggrescore)
        return  aggrescore


    def findAllMatch(self, list1, list2):
        scores = []
        matched = []


        for idx1, item1 in enumerate(list1):
            for idx2, item2 in enumerate(list2):
                p1, v1 = item1
                p2, v2 = item2
                score = self.criteria(v1, v2)
                if score is not 0:
                    if idx1 not in matched and idx2 not in matched:
                        matched.extend([idx1, idx2])
                        scores.append(score)
                    else:
                        if idx1 in matched:
                            idxScore = matched.index(idx1)
                        elif idx2 in matched:
                            idxScore = matched.index(idx2)
                        id2Replace = math.floor(idxScore/2)
                        oldscore = matched[id2Replace]
                        if score > oldscore:
                            matched[id2Replace] = score
                            matched[id2Replace*2] = idx1
                            matched[id2Replace*2+1] = idx2

        return scores





    def aggregateForInstance(self,scores, lenSp, lenTp, wfactor = 0.2):
        return sum(scores) / (len(scores) + wfactor *(lenSp+lenTp - 2* len(scores)))



    def comparePString(self, p1, p2):
        l1,l2 = len(p1),len(p2)
        ll,sl = max(l1,l2),min(l1,l2)
        if ll/sl > 3:
            return 0  #heuristic, eliminate if string length difference too large

        return StringMatcher.jaro_similarity(p1, p2)


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
    def criteria(self, value1, value2):
        #rdflib.term.Literal('Distillate_Oil', datatype=rdflib.term.URIRef('http://www.w3.org/2001/XMLSchema#string'))
        #print('value1 ',value1.value)
        #print('value2 ', value2.value)
        stringSimThre = 0.7
        MarginValue = 0.001



        if self.isNumberType(value1) and self.isNumberType(value2) :
            #todo: contemplate this, should we really penalize type mismatch directly?


            #print('compare value')
            v1,v2 = float(value1.value),float(value2.value)
            l = max(abs(v1), abs(v2))
            if v1 == 0 and v2 == 0 :
                score = 1
            elif l == 0:
                print('werid value ', v1,v2)
                return 0
            else:
                margin = abs(v1-v2)/l
                if margin <= MarginValue:
                   # if self.numberTypeMismatch(value1, value2):
                   #     return 1
                   # else:
                   return 1
                else:
                    return 0
        elif self.isDateType(value1) and self.isDateType(value2):
            score = self.compareDate(value1.value,value2.value)

        elif not self.isNumberType(value1) and not self.isNumberType(value2) and self.isStringType(value1)  and self.isStringType(value2):
            #compare string similarity
            #print('compare string')
            score = self.comparePString(value1.value,value2.value)

        else:
            #print('type mismatch')
            score = 0

        #print('score ',score)
        if score < stringSimThre:
            return 0
        else:
            return score


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
        if v.datatype  is 'http://www.w3.org/2001/XMLSchema#integer' or v.datatype is 'http://www.w3.org/2001/XMLSchema#decimal' or  v.datatype is 'http://www.w3.org/2001/XMLSchema#float' or  v.datatype is 'http://www.w3.org/2001/XMLSchema#double':
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
        dateRegex1 = '^(0?[1-9]|1[012])?[- /.]?(0?[1-9]|[12][0-9]|3[01])?[- /.]?([12]\d{3})?$'
        dateRegex2 = '^([12]\d{3})?[- /.]?(0?[1-9]|1[012])?[- /.]?(0?[1-9]|[12][0-9]|3[01])?$'
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

        return v.datatype is 'http://www.w3.org/2001/XMLSchema#date' or self.matchDate(v.value) is not None
    def isStringType(self,v):
        return v.datatype is 'http://www.w3.org/2001/XMLSchema#string' or isinstance(v.value,str)


if __name__ =="__main__":
    pass