import collections
import json
import pickle
import unittest

import numpy as np

import util

PATH_MATCHES_KWL_GPPD_DEU = './data/matches_kwl_gppd_DEU.csv'
PATH_CONF_AUTO_KWL_GPPD_DEU = './tests/conf/conf_auto_calibration_kwl_gppd_geo.json'

class TestCaseOntoMatch(unittest.TestCase):

    def setUp(self):
        print()
        util.init_logging()
        np.random.seed(1)

    def load_kwl_gppd_ontologies(self):
        with open('./data/kwl_address_211022.pkl','rb') as file:
            src_onto = pickle.load(file)
        with open('./data/gppd_DEU_211022.pkl','rb') as file:
            tgt_onto = pickle.load(file)
        return src_onto, tgt_onto

    def load_kwl_with_geo_coordinates_gppd_ontologies(self):
        with open('./data/kwl_address_geo_211022.pkl','rb') as file:
            src_onto = pickle.load(file)
        with open('./data/gppd_DEU_geo_211022.pkl','rb') as file:
            tgt_onto = pickle.load(file)
        return src_onto, tgt_onto

    def read_conf_kwl(self):
        config_file = './tests/conf/conf_scoring_weight_matcher_kwl_gppd.json'
        with open(config_file) as json_config:
            params = json.load(json_config, object_pairs_hook=collections.OrderedDict)
        return params
