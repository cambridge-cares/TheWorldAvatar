from matchManager import *
from matchers.Penalizer import *
from alignment import Alignment
from PlusImport import PlusImport

import os
import owlready2
import time
import csv


if __name__ == '__main__':

    starttime = time.time()

    matchSteps = ['ValueMatcher','instanceStringMatcher', 'instanceBOWMatcher']
    w = [0.5, 0.4, 0.1]
    paras = [None,None,None]
    threshold = .0
    clist = [('PowerStation', 'PowerPlant', 0.9)]
    sublist = ['RenewablePlant', 'FossilFuelPlant', 'HydroelectricPlant', 'HydrogenPlant', 'NuclearPlant', 'CogenerationPlant', 'GeothermalPlant', 'MarinePlant', 'BiomassPlant', 'WindPlant', 'SolarPlant','WastePlant','PowerPlant']
    for subc in sublist:
        for subc in sublist:
            clist.append((subc,subc,0.9))

    dir = './data/'
    #src = dir + 'kwl.owl'
    #tgt = dir + 'gppd0722.owl'
    src = dir + 'kwl.pkl'
    tgt = dir + 'gppd.pkl'

    m = matchManager(matchSteps, src, tgt, thre=threshold, weight=w, paras=paras,matchIndividuals =True,penalize ={'class':True,'align':Alignment(clist)},useAttrFinder=False)



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
    m.renderResult(" http://dbpedia.org/resource", "http://www.theworldavatar.com", '210927xxx.owl', True)

    timenow = time.time()-starttime
    print('elapsed time in seconds=', timenow)
