#todo: multi:edit,jaro,qgram
import traceback

from nltk.metrics.distance import jaro_similarity,edit_distance
import sys
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
        return StringMatcher.jaro_similarity(e1, e2)

    @classmethod
    def jaro_similarity(cls, a1, a2):
        return jaro_similarity(a1, a2)

    @classmethod
    def edit_similarity(cls,a1,a2):
        return 1 - edit_distance(a1, a2) / (max(len(a1), len(a2)))
    @classmethod
    def stringCompareEdit(cls, a1, a2):
        return edit_distance(a1, a2)/max(len(a1),len(a2))


if __name__ == '__main__':

    #addr = "C:/Users/Shaocong/WORK/ontoMatchData/simMatch/test/germany/Altbach_Coal_Power_Plant_Germany.owl"
    #addr2 = "http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl"
    #addr3 = "http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl"

    #temp = PlusImport( [addr,addr2, addr3])
    #on = Ontology(temp.name)
    #print('edn')

    #for i,d in o.tokensDict.items():
    #    print('!!!'+o.entities[i]+'    ' +str(d))
    #print(str(len(o.entities)))
    fileUrl = sys.argv[1]
    srcUrl = sys.argv[2]
    tgtUrl = sys.argv[3]
    match_method = sys.argv[4]

    try:
        srcPi = open(srcUrl, 'rb')
        srcOnto = pickle.load(srcPi)
        tgtPi = open(srcUrl, 'rb')
        tgtOnto = pickle.load(srcPi)
        matcher = StringMatcher((srcOnto, tgtOnto))
        mm = getattr(matcher, match_method)
        #matchSerial for T, matchIndividual for I
        a = mm()
        a.render(fileUrl)
        print("success")
    except Exception:
        print(repr(Exception))
        print(traceback.print_exc())
