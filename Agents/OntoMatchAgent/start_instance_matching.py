import logging
import time

import coordinator
import util

if __name__ == '__main__':

    util.init_logging('.', '..')

    starttime = time.time()

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
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_address.owl'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_address_no_geo.pkl'
    srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_address_geo.pkl'
    #srcaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/kwl.pkl'
    #srcaddr = './data/kwl_geo.pkl'

    #tgtaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/gppd0722.owl'
    tgtaddr = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/gppd0722.pkl'
    #tgtaddr = './data/gppd.pkl'


    # DBPEDIA DEU vs GPPD DEU (Germany)
    # -------------------------

    #srcaddr = 'C:/Users/freig/Downloads/dbpedia_DEU_Altbach_only.rdf'
    #tgtaddr = 'C:/Users/freig/Downloads/gppd_DEU_Altbach_only.owl'

    #srcaddr = 'C:/Users/freig/Downloads/sparql_2021-10-11_13-06-46Z.rdf'
    #srcaddr = 'C:/my/tmp/ontomatch/tmp_kwl_files/dbpedia_DEU_211010.pkl'
    #tgtaddr = './data/gppd.pkl'

    #srcaddr = 'C:/my/tmp/ontomatch/dbpedia_DEU_converted_ontopowsys.owl'
    #tgtaddr = './data/gppd.pkl'

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
                }
        ]},
        "matching": {
            #"name": "matchManager.matchManager",
            #"model_specific": {
            #    "steps": ["ValueMatcher", "instanceStringMatcher", "instanceBOWMatcher"],
            #    "weights": [0.5, 0.4, 0.1],
            #    "params": [None, None, None],
            #    "threshold": -1.,
            #},

            "name": "coordinator.InstanceMatcherWithAutoCalibration",
            "model_specific": {
            },
        }
    }


    agent = coordinator.Agent()
    agent.start(params)

    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)
