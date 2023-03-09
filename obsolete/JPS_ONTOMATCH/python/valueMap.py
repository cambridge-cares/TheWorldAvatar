import rdflib


class valueMap(object):
    def __init__(self, ids,map, opts ={}):
        #todo
        self.ids = ids
        self.map = map #s, p, v,tree tuple
        self.allInstanceNum = self.getAllInstanceNum()
        if hasattr(opts, 'useWeight') and opts.useWeight :#option to use weight
            self.pwMap = self.calWeight(onto)



    def items(self):
        return list(self.map.items())
    def getAllInstanceNum(self):
        return len(self.map.keys())

    def getPInstanceNum(self,p):
        return len(self.searchProperty(p))

    def searchInstance(self,e):
        '''
        search for a particular instance
        :param e:
        :return:
        '''
        id = self.ids.index(e)
        return self.map[id]

    def searchProperty(self,p):
        pInstances = []
        for s, pair in enumerate(self.map.items()):
            if pair[0] is p:
                pInstances.append(pair[1])

        return pInstances






    def calWeight(self, onto):
        '''
        calculate weight for each p
        Method in <ontology instance matching by considering semantic link cloud>

        :return:
        '''
        self.weightMap = dict()
        for p in onto.data_properties():
            self.weightMap[p] = self.getPInstanceNum(p)/self.allInstanceNum



    def IPF(self):
        '''
        weight By IPF
        :return:
        '''
        pass






if __name__ == "__main__":
    from owlready2 import *
    from ontologyWrapper import PlusImport
    addr = "C:/Users/Shaocong/WORK/ontoMatchData/simMatch/test/59th_Street_Oil_Plant_NY_USA.owl"
    addr2 = "C:/Users/Shaocong/WORK/ontoMatchData/simMatch/test/PowerPlant.owl"
    addr3 = "C:/Users/Shaocong/WORK/ontoMatchData/relatedOwls/system_v1.owl"

    monto = get_ontology(addr).load()
    print('show before individuals')
    print(list(monto.individuals()))
    print(list(monto.classes()))

    a  =PlusImport(monto)
    onto = a.onto

    print('show individuals')
    print(list(onto.individuals()))
    print(list(onto.properties()))
    l = list(onto.properties())

    e = onto.search(consumesPrimaryFuel="*")
    print(e)
