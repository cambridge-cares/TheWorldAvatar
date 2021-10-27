import logging
import pickle
import unittest

import util

class TestCaseOntoMatch(unittest.TestCase):

    def setUp(self):
        print()
        util.init_logging('.', '..')

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
