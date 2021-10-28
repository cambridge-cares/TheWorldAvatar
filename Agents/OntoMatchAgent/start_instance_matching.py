import argparse
import collections
import json
import logging
import os
import random
import time

import numpy as np

import coordinator
import util

#TODO-AE remove this old method
'''
def start_OLD():
    util.init_logging('.', '..')

    # DUKES vs. GPPD GBR (United Kingdom)
    # -----------------------------------
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/dukes_geo_wannie.owl'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/dukes_geo_wannie.pkl'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/dukes_no_geo.owl'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/dukes_no_geo.pkl'

    #tgtaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/gppd_GBR.owl'
    #tgtaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/gppd_GBR.pkl'

    # KWL vs GPPD DEU (Germany)
    # -------------------------

    #srcaddr = './data/kwl_address_211022.ttl'
    srcaddr = './data/kwl_address_211022.pkl'

    #tgtaddr = './data/gppd_DEU_211022.ttl'
    tgtaddr = './data/gppd_DEU_211022.pkl'


    # DBPEDIA DEU vs GPPD DEU (Germany)
    # -------------------------

    #srcaddr = 'C:/Users/freig/Downloads/dbpedia_DEU_Altbach_only.rdf'
    #tgtaddr = 'C:/Users/freig/Downloads/gppd_DEU_Altbach_only.owl'

    #srcaddr = 'C:/Users/freig/Downloads/sparql_2021-10-11_13-06-46Z.rdf'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/dbpedia_DEU_211010.pkl'
    #tgtaddr = './data/gppd_DEU_211022.pkl'

    #srcaddr = 'C:/my/tmp/ontomatch/dbpedia_DEU_converted_ontopowsys.owl'
    #tgtaddr = './data/gppd_DEU_211022.pkl'

    params = {
        "dataset": {
            "src": srcaddr,
            "tgt": tgtaddr,
        },
        "pre_processing": {
            #"add_knowledge": None,
            #"add_knowledge": "knowledge.geoNames",
            "add_knowledge": "knowledge.geocoding",
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
                 #"blocking_properties": ['dbp:name', 'dbp:owner', "name", "isOwnedBy/hasName"], #DBPedia
                 "reset_index": False,
            }
        },
        "mapping": {
                "mode": "auto",
                'similarity_functions': [{
                    "name": "dist_nltk_edit",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 3
                },{
                    "name": "dist_absolute",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 10
                },{
                    "name": "dist_relative",
                    "cut_off_mode": "fixed"
                },{
                    "name": "dist_equal",
                    "cut_off_mode": "fixed"
                },{
                    "name": "dist_cosine_with_tfidf",
                    "cut_off_mode": "fixed"
            }
        ]},
        "matching": {
            "name": "matchManager.matchManager",
            "model_specific": {
                "steps": ["ValueMatcher", "instanceStringMatcher", "instanceBOWMatcher"],
                "weights": [0.5, 0.4, 0.1],
                "params": [None, None, None],
                "threshold": -1.,
            },

            #"name": "coordinator.InstanceMatcherWithAutoCalibration",
            #"model_specific": {
            #},
        }
    }

    starttime = time.time()
    agent = coordinator.Agent()
    agent.start(params)
    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)
'''

def start(config_dev=None):

    print('current working directory=', os.getcwd())

    parser = argparse.ArgumentParser()
    parser.add_argument('--config', type=str, default=None)
    args = parser.parse_args()

    if args.config:
        with open(args.config) as json_config:
            config = json.load(json_config, object_pairs_hook=collections.OrderedDict)
    else:
        config = config_dev

    seed = config['numerical_settings'].get('seed')
    np.random.seed(seed)
    random.seed(seed)

    util.init_logging('.', '..')
    logging.info('current working directory=%s', os.getcwd())
    logging.info('args=%s', args)
    logging.info('config=%s', config)

    starttime = time.time()
    agent = coordinator.Agent()
    agent.start(config)
    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)


if __name__ == '__main__':
    start()

