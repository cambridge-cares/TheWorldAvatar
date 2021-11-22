import collections
import json
import pickle
import unittest

import numpy as np

import ontomatch.utils.util

PATH_MATCHES_PP_DEU = './data/matches_power_plant_DEU.csv'
PATH_CONF_PP_DEU_AUTO = './tests/conf/conf_power_plant_DEU_auto.json'
PATH_CONF_PP_DEU_AUTO_GEO = './tests/conf/conf_power_plant_DEU_auto_geo.json'

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

    def read_conf_kwl(self):
        config_file = './tests/conf/conf_power_plant_DEU_weight.json'
        with open(config_file) as json_config:
            params = json.load(json_config, object_pairs_hook=collections.OrderedDict)
        return params
