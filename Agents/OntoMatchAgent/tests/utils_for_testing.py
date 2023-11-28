import collections
import json
import logging
import pickle
import unittest

import numpy as np

import ontomatch.evaluate
import ontomatch.utils.util

PATH_MATCHES_PP_DEU = './data/power_plant_DEU/matches_power_plant_DEU.csv'
PATH_CONF_PP_DEU_AUTO = './tests/conf/conf_power_plant_DEU_auto.json'
PATH_CONF_PP_DEU_AUTO_GEO = './tests/conf/conf_power_plant_DEU_auto_geo.json'
PATH_CONF_PP_DEU_XGB = './tests/conf/conf_power_plant_DEU_xgb.json'
PATH_CONF_PP_DEU_WEIGHT = './tests/conf/conf_power_plant_DEU_weight.json'

class TestCaseOntoMatch(unittest.TestCase):

    def setUp(self):
        print()
        ontomatch.utils.util.init_logging()
        np.random.seed(1)

    def load_kwl_gppd_ontologies(self):
        with open('./data/power_plant_DEU/kwl.pkl','rb') as file:
            src_onto = pickle.load(file)
        with open('./data/power_plant_DEU/gppd_DEU.pkl','rb') as file:
            tgt_onto = pickle.load(file)
        return src_onto, tgt_onto

    def load_kwl_with_geo_coordinates_gppd_ontologies(self):
        with open('./data/power_plant_DEU/kwl_geo.pkl','rb') as file:
            src_onto = pickle.load(file)
        with open('./data/power_plant_DEU/gppd_DEU.pkl','rb') as file:
            tgt_onto = pickle.load(file)
        return src_onto, tgt_onto

    def read_params(self, file, symmetric=False):
        with open(file) as json_config:
            params = json.load(json_config, object_pairs_hook=collections.OrderedDict)
        # don't dump score files when running tests
        params['post_processing']['dump'] = None
        if symmetric:
            params['matching']['model_specific']['symmetric'] = True
        return params

    def read_kwl_gppd_DEU_matching_file(self):
        matchfile = PATH_MATCHES_PP_DEU
        index_set_matches = ontomatch.evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
        logging.info('ground truth matches=%s', len(index_set_matches))
        return index_set_matches
