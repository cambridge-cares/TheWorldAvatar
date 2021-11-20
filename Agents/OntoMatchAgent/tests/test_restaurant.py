import logging
import pickle

import ontomatch.blocking
import ontomatch.scoring
import tests.utils_for_testing

class TestRestaurant(tests.utils_for_testing.TestCaseOntoMatch):

    def load_restaurant_ontologies(self):
        with open('./data/restaurant/zagats.pkl','rb') as file:
            src_onto = pickle.load(file)
        with open('./data/restaurant/fodors.pkl','rb') as file:
            tgt_onto = pickle.load(file)
        return src_onto, tgt_onto

    def get_restaurant_params_blocking(self):
        return {
                "name": "TokenBasedPairIterator",
                "model_specific": {
                     "min_token_length": 3,
                     "max_token_occurrences_src": 20,
                     "max_token_occurrences_tgt": 20,
                     "blocking_properties": ["address/streetAddress"],
                    "reset_index": False,
                }
        }

    def test_configure_score_fct_cosine_with_tfidf(self):
        params_sim_fcts = [{
                "name": "dist_cosine_with_tfidf",
                "cut_off_mode": "fixed"
            }
        ]
        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)

        # prepare df_index_tokens before using cosine_with_tfidf
        src_onto, tgt_onto = self.load_restaurant_ontologies()
        params_blocking = self.get_restaurant_params_blocking()
        ontomatch.blocking.create_iterator(src_onto, tgt_onto, params_blocking)

        df_index_tokens = ontomatch.blocking.TokenBasedPairIterator.df_index_tokens_unpruned
        logging.debug('number of index tokens=%s', len(df_index_tokens))
        candidate_pairs = ontomatch.blocking.TokenBasedPairIterator.candidate_matching_pairs
        self.assertEqual(len(candidate_pairs), 1186)

        entity1 = {'type': ['Restaurant'], 'pos': 37, 'name': 'Virgil_s_Real', 'address/addressLocality': 'New York City', 'https://schema.org/servesCuisine': 'BBQ', 'address/telephone': '212-921-9494', 'address/streetAddress': '152 W. 44th St.'}
        entity2 = {'type': ['Restaurant'], 'pos': 17, 'name': 'Osteria_al', 'address/telephone': '212/944-3643', 'address/addressLocality': 'New York', 'https://schema.org/servesCuisine': 'Italian', 'address/streetAddress': '142 W. 44th St.'}

        prop_prop_sim_tuples = [
            ('address/streetAddress', 'address/streetAddress', sim_fcts[0]),
        ]

        result = ontomatch.scoring.ScoreManager.calculate_between_entities(entity1, entity2,  prop_prop_sim_tuples)
        self.assertAlmostEqual(result[0], 0.1546, places=2)
