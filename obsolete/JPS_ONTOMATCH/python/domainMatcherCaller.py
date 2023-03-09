from ontologyWrapper import *
import sys
import pickle
from matchers.domainMatcher import DomainMatcher
import traceback

if __name__ == '__main__':
    fileUrl = sys.argv[1]
    srcUrl = sys.argv[2]
    tgtUrl = sys.argv[3]
    match_method = sys.argv[4]
    model_addr = sys.argv[5]
    dict_addr = sys.argv[6]

    try:
        srcPi = open(srcUrl, 'rb')
        print('src url: '+srcUrl)
        srcOnto = pickle.load(srcPi)
        tgtPi = open(tgtUrl, 'rb')
        tgtOnto = pickle.load(tgtPi)
        matcher = DomainMatcher((srcOnto, tgtOnto),(model_addr, dict_addr))
        mm = getattr(matcher, match_method)
        # matchSerial for T, matchIndividual for I
        a = mm()
        a.id2Entity(srcOnto,tgtOnto)
        a.render(srcUrl,tgtUrl,fileUrl)
        print("success")
    except Exception:
        print(repr(Exception))
        print(traceback.print_exc())
