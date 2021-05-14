from ontologyWrapper import *
import sys
import pickle
import matchers.instanceStringMatcher
import traceback
if __name__ == '__main__':

    fileUrl = sys.argv[1]
    srcUrl = sys.argv[2]
    tgtUrl = sys.argv[3]
    match_method = sys.argv[4]

    try:
        srcPi = open(srcUrl, 'rb')
        srcOnto = pickle.load(srcPi)
        tgtPi = open(tgtUrl, 'rb')
        tgtOnto = pickle.load(tgtPi)
        matcher = matchers.instanceStringMatcher((srcOnto, tgtOnto))
        mm = getattr(matcher, match_method)
        # matchSerial for T, matchIndividual for I
        a = mm()
        a.id2Entity(srcOnto,tgtOnto,'individualList')
        a.render(srcUrl,tgtUrl,fileUrl)
        print("success")
    except Exception:
        print(repr(Exception))
        print(traceback.print_exc())

