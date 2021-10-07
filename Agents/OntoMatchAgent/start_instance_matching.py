import logging
import time

from alignment import Alignment
import coordinator
from matchManager import matchManager
import util

if __name__ == '__main__':

    util.init_logging('.', '..')

    starttime = time.time()

    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_address.owl'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_address_no_geo.pkl'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_address_geo.pkl'
    #srcaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/kwl.pkl'
    #tgtaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/gppd0722.owl'
    #tgtaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/gppd0722.pkl'
    srcaddr = './data/kwl_geo.pkl'
    tgtaddr = './data/gppd.pkl'

    params = {
        "dataset": {
            "src": srcaddr,
            "tgt": tgtaddr,
        },
        "pre_processing": {
            "add_knowledge": True,
            "pickle_dump": False,
        },
        "blocking": {
            #"name": "FullPairIterator",
            "name": "TokenBasedPairIterator",
            "model_specific": {
                "min_token_length": 3,
                "max_token_occurrences_src": 20,
                "max_token_occurrences_tgt": 20,
                "blocking_properties": ["name", "isOwnedBy/hasName"],
                "reset_index": False,
            }
        },
        "matching": {
            "name": "matchManager",
            "model_specific": {
                "steps": ["ValueMatcher", "instanceStringMatcher", "instanceBOWMatcher"],
                "weights": [0.5, 0.4, 0.1],
                "params": [None, None, None],
                "threshold": 0.2,
            },
        }
    }

    # TODO-AE Do we need this for instance matching?
    clist = [('PowerStation', 'PowerPlant', 0.9)]
    sublist = ['RenewablePlant', 'FossilFuelPlant', 'HydroelectricPlant', 'HydrogenPlant', 'NuclearPlant', 'CogenerationPlant', 'GeothermalPlant', 'MarinePlant', 'BiomassPlant', 'WindPlant', 'SolarPlant','WastePlant','PowerPlant']
    for subc in sublist:
        #for subc in sublist:
        clist.append((subc,subc,0.9))
    penalize = {'class':True,'align':Alignment(clist)}

    agent = coordinator.Agent()
    agent.start(params, penalize)

    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)
