import pickle
import unittest

import blocking

class TestModels(unittest.TestCase):

    def load_kwl_gppd_ontologies(self):
        with open('./data/kwl.pkl','rb') as file:
            src_onto = pickle.load(file)
        with open('./data/gppd.pkl','rb') as file:
            tgt_onto = pickle.load(file)
        return src_onto, tgt_onto

    def test_fullpairiterator(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        #it = blocking.FullPairIterator(src_onto, tgt_onto)
        params = {'name': 'FullPairIterator'}
        it = blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in it:
            count += 1
        assert count == len(src_onto.individualList) * len(tgt_onto.individualList)
        assert len(it) == count

    def test_tokenbasedpairiterator_max20(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        #it = blocking.TokenBasedPairIterator(src_onto, tgt_onto,
        #        min_token_length=3, max_token_occurrences_src=20, max_token_occurrences_tgt=20, reset_index=True)
        params = {
            'name': 'TokenBasedPairIterator',
            'min_token_length': 3,
            'max_token_occurrences_src': 20,
            'max_token_occurrences_tgt': 20,
            'reset_index': True
        }
        it = blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in it:
            count += 1
        assert count == 4704
        assert len(it) == count

    def test_tokenbasedpairiterator_max30(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        #it = blocking.TokenBasedPairIterator(src_onto, tgt_onto,
        #        min_token_length=3, max_token_occurrences_src=30, max_token_occurrences_tgt=30, reset_index=True)
        params = {
            'name': 'TokenBasedPairIterator',
            'min_token_length': 3,
            'max_token_occurrences_src': 30,
            'max_token_occurrences_tgt': 30,
            'reset_index': True
        }
        it = blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in it:
            count += 1
        assert count == 6972
        assert len(it) == count
