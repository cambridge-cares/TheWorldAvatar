import sys
from ontologyWrapper import *
import pickle
import matchers.StringMatcher
import traceback
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
    #srcUrl = "D://workwork/ontoMatchFiles/sourceOntology.pkl"
    #tgtUrl  ="D://workwork/ontoMatchFiles/targetOntology.pkl"
    #match_method = "matchSerial"
    #fileUrl = "http://www.test.com"
    try:
        tgtPi = open(tgtUrl, 'rb')
        print(tgtPi)
        print('save to here: '+fileUrl)
        tgtOnto = pickle.load(tgtPi)
        srcPi = open(srcUrl, 'rb')
        srcOnto = pickle.load(srcPi)
        matcher = matchers.StringMatcher((srcOnto, tgtOnto))
        mm = getattr(matcher, match_method)
        #matchSerial for T, matchIndividual for I
        a = mm()
        a.id2Entity(srcOnto,tgtOnto)
        a.render(srcUrl,tgtUrl,fileUrl)
        print("success")
    except Exception:
        print(repr(Exception))
        print(traceback.print_exc())
