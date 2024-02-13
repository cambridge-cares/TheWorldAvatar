import logging

import pandas as pd

import ontomatch.blocking
import tests.utils_for_testing

class TestBlocking(tests.utils_for_testing.TestCaseOntoMatch):

    def test_fullpairiterator(self):
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params = {'name': 'FullPairIterator'}
        iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, len(src_onto) * len(tgt_onto))
        self.assertEqual(len(iterator), count)

    def test_tokenbasedpairiterator_max20(self):
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 20,
                'max_token_occurrences_tgt': 20,
                'blocking_properties': ['name', 'owner'],
                'reset_index': True,
            }
        }
        iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, 4719)
        self.assertEqual(len(iterator), count)

    def test_tokenbasedpairiterator_max30(self):
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 30,
                'max_token_occurrences_tgt': 30,
                'blocking_properties': ['name', 'owner'],
                'reset_index': True,
            }
        }
        iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, 7371)
        self.assertEqual(len(iterator), count)


    def test_create_token_index_assert_Sandhofer(self):
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        
        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 20,
                'max_token_occurrences_tgt': 20,
                'blocking_properties': ['name', 'owner'],
                'reset_index': True,
            }
        }

        ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        df_index_tokens = ontomatch.blocking.TokenBasedPairIterator.df_index_tokens

        #logging.info(df_index_tokens.to_string())

        logging.info('\n%s', df_index_tokens.loc['berlin'])
        logging.info('\n%s', df_index_tokens.loc['medienversorgung'])
        logging.info('\n%s', df_index_tokens.loc['sandhofer'])

    def test_tokenbasedpairiterator_max20_for_restaurant_csv_files(self):
        src_onto, tgt_onto = self.read_restaurant_tables()

        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 20,
                'max_token_occurrences_tgt': 20,
                'blocking_properties': ['name', 'addr', 'phone'],
                'reset_index': True,
            }
        }
        iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, 2945)
        self.assertEqual(len(iterator), count)