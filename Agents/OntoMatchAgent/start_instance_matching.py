import logging
import time

from alignment import Alignment
import coordinator
from matchManager import matchManager
import util

if __name__ == '__main__':

    util.init_logging('.', '..')

    starttime = time.time()

    matchSteps = ['ValueMatcher','instanceStringMatcher', 'instanceBOWMatcher']
    w = [0.5, 0.4, 0.1]
    paras = [None,None,None]
    #threshold = .2
    threshold = .0
    clist = [('PowerStation', 'PowerPlant', 0.9)]
    sublist = ['RenewablePlant', 'FossilFuelPlant', 'HydroelectricPlant', 'HydrogenPlant', 'NuclearPlant', 'CogenerationPlant', 'GeothermalPlant', 'MarinePlant', 'BiomassPlant', 'WindPlant', 'SolarPlant','WastePlant','PowerPlant']
    for subc in sublist:
        #for subc in sublist:
        clist.append((subc,subc,0.9))

    srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_without_geo.pkl'
    tgtaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/gppd0722.pkl'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl.owl'
    #srcaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/kwl.owl'
    #tgtaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/gppd0722.owl'
    #srcaddr = './data/kwl.pkl'
    #tgtaddr = './data/gppd.pkl'

    #agent = coordinator.Agent()
    #srconto, tgtonto = agent.load(srcaddr, tgtaddr, dump_ontology=False)

    m = matchManager(matchSteps, srcaddr, tgtaddr, thre=threshold, weight=w, paras=paras,matchIndividuals =True,penalize ={'class':True,'align':Alignment(clist)},useAttrFinder=False)
    #m = matchManager(matchSteps, srconto, tgtonto, thre=threshold, weight=w, paras=paras,matchIndividuals =True,penalize ={'class':True,'align':Alignment(clist)},useAttrFinder=False)



    #params_blocking = {'name': 'FullPairIterator'}

    params_blocking = {
        'name': 'TokenBasedPairIterator',
        'min_token_length': 3,
        'max_token_occurrences_src': 20,
        'max_token_occurrences_tgt': 20,
        'blocking_properties': ['name', 'isOwnedBy'],
        'reset_index': False
    }




    params = {
        'blocking': params_blocking
    }

    a = m.runMatch("matchWrite2Matrix", to1=False, rematch = False, params = params)

    #m.showResult(m.A,'individualList')
    m.renderResult(" http://dbpedia.org/resource", "http://www.theworldavatar.com", '2109xx.owl', True)

    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)
