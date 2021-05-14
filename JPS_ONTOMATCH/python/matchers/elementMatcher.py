import multiprocessing as mp
from alignment import Alignment
import pathos.multiprocessing as mpp
'''
matcher that compares entities pair by pair
'''

class ElementMatcher(object):

    def __init__(self, es, penalizer = None):
        self.S, self.T = es
        self.compareMethod = 'compare'
        self.prep()


    def prep(self):
        pass

    def match(self):
        '''
        :return: alignment
        '''
        #todo:parallel
        print(self.name)
        idList = []

        for idSrc in range(len(self.S.entities)):
            for idTgt in range(len(self.T.entities)):
                idList.append((idSrc, idTgt))
        pool =  mpp.Pool(mp.cpu_count())

        results =pool.map(self.add2Alignment, idList)

        self.a = Alignment(results)

        return a


    def matchIndividuals(self):
        results = []
        for keyS in range(len(self.S.individualList)):
            for keyT in range(len(self.T.individualList)):
                #todo:class check??
                #todo:build a list of ids, now is string id
                #property check
                results.append(self.add2Alignment((keyS, keyT), self.compareMethod))
        #print(results)
        a = Alignment(results)
        return a

    def matchSerial(self):
        '''
        serial version of main func for testing
        :return:
        '''
        results = []
        for idSrc in range(len(self.S.entities)):
            for idTgt in range(len(self.T.entities)):
                #todo: typeCheck
                if self.S.types[idSrc] == self.T.types[idTgt]:
                    results.append(self.add2Alignment((idSrc, idTgt),self.compareMethod))



        a = Alignment(results)
        print('finish matching step')
        return a

    #todo: rematching logic: only update class
    def rematch(self, a, method = 'compare',types = ['class', 'property']):
        results = []
        for idSrc, idTgt, p in a.map:
            if self.S.types[idSrc] == self.T.types[idTgt] and self.S.types[idSrc] in types and self.T.types[idTgt] in types:
                results.append(self.add2Alignment((idSrc, idTgt), method))
        a.update(results)
        print('finish')
        print(a.map)
        return a


    def add2Alignment(self, para, method = 'compare'):
        #print('!!!!!!!!!!!'+str(para))
        #print(self.compare)
        m = getattr(self, method)
        return para[0], para[1],m(*para)






    def compare(self, e1, e2):
        print('PASS')
        return 'pass'







if __name__ == "__main__":
    a=ElementMatcher(1)
    print(a)
