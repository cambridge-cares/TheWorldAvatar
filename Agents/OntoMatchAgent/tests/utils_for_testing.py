import collections
import json
import logging
import unittest

import numpy as np

import ontomatch.evaluate
import ontomatch.utils.util

PATH_MATCHES_PP_DEU = './tests/data/matches_power_plant_DEU.csv'
PATH_TEST_PP_DEU = './tests/data/KWL-GPPDdeu/test.csv'
#PATH_MATCHES_REST = './tests/data/restaurant/all.csv'
PATH_TEST_REST = './tests/data/restaurant/test.csv'
PATH_CONF_PP_DEU_AUTO = './tests/conf/conf_power_plant_DEU_auto.json'
PATH_CONF_PP_DEU_AUTO_CSV = './tests/conf/conf_power_plant_DEU_auto_csv.json'
PATH_CONF_PP_DEU_AUTO_GEO = './tests/conf/conf_power_plant_DEU_auto_geo.json'
#PATH_CONF_PP_DEU_XGB = './tests/conf/conf_power_plant_DEU_xgb.json'
PATH_CONF_PP_DEU_XGB_CSV = './tests/conf/conf_power_plant_DEU_xgb_csv.json'
#PATH_CONF_PP_DEU_WEIGHT = './tests/conf/conf_power_plant_DEU_weight.json'
PATH_CONF_PP_DEU_WEIGHT_CSV = './tests/conf/conf_power_plant_DEU_weight_csv.json'
PATH_CONF_REST_AUTO_CSV = './tests/conf/conf_restaurant_auto_csv.json'

class TestCaseOntoMatch(unittest.TestCase):

    def setUp(self):
        print()
        ontomatch.utils.util.init_logging()
        np.random.seed(1)

    #def load_kwl_gppd_ontologies(self):
    #    with open('./data/power_plant_DEU/kwl.pkl','rb') as file:
    #        src_onto = pickle.load(file)
    #    with open('./data/power_plant_DEU/gppd_DEU.pkl','rb') as file:
    #        tgt_onto = pickle.load(file)
    #    return src_onto, tgt_onto
    
    def read_kwl_gppd_tables(self):
        src_df = ontomatch.utils.util.read_csv_table_ditto_format('./tests/data/KWL-GPPDdeu/tableA.csv')
        tgt_df = ontomatch.utils.util.read_csv_table_ditto_format('./tests/data/KWL-GPPDdeu/tableB.csv')
        return src_df, tgt_df

    #def load_kwl_with_geo_coordinates_gppd_ontologies(self):
    #    with open('./data/power_plant_DEU/kwl_geo.pkl','rb') as file:
    #        src_onto = pickle.load(file)
    #    with open('./data/power_plant_DEU/gppd_DEU.pkl','rb') as file:
    #        tgt_onto = pickle.load(file)
    #    return src_onto, tgt_onto

    def read_params(self, file):
        with open(file) as json_config:
            params = json.load(json_config, object_pairs_hook=collections.OrderedDict)
        # don't dump score files when running tests
        params['post_processing']['dump'] = None
        return params

    def read_kwl_gppd_DEU_matching_file(self):
        matchfile = PATH_MATCHES_PP_DEU
        index_set_matches = ontomatch.evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
        logging.info('ground truth matches=%s', len(index_set_matches))
        return index_set_matches

    def read_matches(self, test_file):
        union = ontomatch.utils.util.read_and_concat_csv_table_ditto_format(test_file)
        index_set_matches = union[union['label'] == 1].index
        return index_set_matches    

    def read_klw_gppd_DEU_matches(self):
        return self.read_matches(PATH_TEST_PP_DEU)

    #def read_restaurant_matching_file(self):
    #    matchfile = PATH_MATCHES_REST
    #    index_set_matches = ontomatch.evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
    #    logging.info('ground truth matches=%s', len(index_set_matches))
    #    return index_set_matches
    
    def read_restaurant_matches(self):
        return self.read_matches(PATH_TEST_REST)

    def read_restaurant_tables(self):
        src_df = ontomatch.utils.util.read_csv_table_ditto_format('./tests/data/restaurant/tableA.csv')
        tgt_df = ontomatch.utils.util.read_csv_table_ditto_format('./tests/data/restaurant/tableB.csv')
        return src_df, tgt_df