import blocking
import utils_for_testing

class TestBlocking(utils_for_testing.TestCaseOntoMatch):

    def test_fullpairiterator(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        #iter = blocking.FullPairIterator(src_onto, tgt_onto)
        params = {'name': 'FullPairIterator'}
        iterator = blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, len(src_onto.individualList) * len(tgt_onto.individualList))
        self.assertEqual(len(iterator), count)

    def test_tokenbasedpairiterator_max20(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        #iter = blocking.TokenBasedPairIterator(src_onto, tgt_onto,
        #        min_token_length=3, max_token_occurrences_src=20, max_token_occurrences_tgt=20, reset_index=True)
        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 20,
                'max_token_occurrences_tgt': 20,
                'blocking_properties': ['name', 'isOwnedBy'],
                'reset_index': True,
            }
        }
        iterator = blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, 2432)
        self.assertEqual(len(iterator), count)

    def test_tokenbasedpairiterator_max30(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        #it = blocking.TokenBasedPairIterator(src_onto, tgt_onto,
        #        min_token_length=3, max_token_occurrences_src=30, max_token_occurrences_tgt=30, reset_index=True)
        params = {
            'name': 'TokenBasedPairIterator',
            'model_specific': {
                'min_token_length': 3,
                'max_token_occurrences_src': 30,
                'max_token_occurrences_tgt': 30,
                'blocking_properties': ['name', 'isOwnedBy'],
                'reset_index': True,
            }
        }
        iterator = blocking.create_iterator(src_onto, tgt_onto, params)
        count = 0
        for _, _ in iterator:
            count += 1
        self.assertEqual(count, 3083)
        self.assertEqual(len(iterator), count)
