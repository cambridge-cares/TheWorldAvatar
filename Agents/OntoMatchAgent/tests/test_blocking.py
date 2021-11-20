import logging

import ontomatch.blocking
import tests.utils_for_testing

class TestBlocking(tests.utils_for_testing.TestCaseOntoMatch):

    def test_fullpairiterator(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params = {'name': 'FullPairIterator'}
        iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, len(src_onto.individualList) * len(tgt_onto.individualList))
        self.assertEqual(len(iterator), count)

    def test_tokenbasedpairiterator_max20(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 20,
                'max_token_occurrences_tgt': 20,
                'blocking_properties': ['name', 'isOwnedBy/hasName'],
                'reset_index': True,
            }
        }
        iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, 4726)
        self.assertEqual(len(iterator), count)

    def test_tokenbasedpairiterator_max30(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 30,
                'max_token_occurrences_tgt': 30,
                'blocking_properties': ['name', 'isOwnedBy/hasName'],
                'reset_index': True,
            }
        }
        iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, 7326)
        self.assertEqual(len(iterator), count)

    def test_create_dataframe_assert_Windplant(self):
        ontosrc, _ = self.load_kwl_gppd_ontologies()
        dfsrc = ontomatch.blocking.create_dataframe_from_ontology(ontosrc)
        columns =  [ str(c) for c in dfsrc.columns ]
        logging.info('columns=%s', columns)
        self.assertIn('isOwnedBy/hasName', columns)
        self.assertIn('type', columns)
        rdf_types = []
        for _, row in dfsrc.iterrows():
            for t in row['type']:
                for c in t.split(' '):
                    if c not in rdf_types:
                        rdf_types.append(c)
        logging.info('unique rdf types=%s', rdf_types)
        self.assertIn('WindPlant', rdf_types)

    def test_create_token_index_assert_Sandhofer(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()

        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 20,
                'max_token_occurrences_tgt': 20,
                'blocking_properties': ['name', 'isOwnedBy/hasName'],
                'reset_index': True,
            }
        }

        ontomatch.blocking.create_iterator(src_onto, tgt_onto, params)
        df_index_tokens = ontomatch.blocking.TokenBasedPairIterator.df_index_tokens

        #logging.info(df_index_tokens.to_string())

        logging.info('\n%s', df_index_tokens.loc['berlin'])
        logging.info('\n%s', df_index_tokens.loc['medienversorgung'])
        logging.info('\n%s', df_index_tokens.loc['sandhofer'])
