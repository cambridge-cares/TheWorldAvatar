import sys
from ontologyWrapper import *
import pickle
import matchers.StringMatcher
import traceback
if __name__ == '__main__':

    fileUrl = sys.argv[1]
    srcUrl = sys.argv[2]
    tgtUrl = sys.argv[3]
    match_method = sys.argv[4]
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
