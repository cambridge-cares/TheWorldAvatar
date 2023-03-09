import logging

import pandas as pd

import ontomatch.blocking
import ontomatch.utils.util
import tests.utils_for_testing

class TestUtil(tests.utils_for_testing.TestCaseOntoMatch):

    def test_train_test_split(self):

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
        pairs = iterator.candidate_matching_pairs
        logging.debug('number of candidate pairs=%s', len(pairs))

        index_matches = self.read_kwl_gppd_DEU_matching_file()
        df_sim_vecs = ontomatch.utils.util.read_csv('./tests/data/sim_vectors_kg_20.csv')
        columns_x = ['0', '1', '2']

        columns_for_saving = ['y']
        for train_size in [0., 0.1, 0.8]:
            column_ml_phase = 'ml_phase_' + str(train_size)
            columns_for_saving.append(column_ml_phase)
            df_candidate_pairs, x_train, x_test, y_train, y_test = ontomatch.utils.util.train_test_split(
                    df_sim_vecs, index_matches, train_size, columns_x, column_ml_phase=column_ml_phase)

            train_count = int(train_size * len(df_sim_vecs))
            self.assertEqual(len(x_test), len(df_candidate_pairs) -  train_count)
            self.assertEqual(len(y_test), len(df_candidate_pairs) -  train_count)
            if train_size > 0:
                self.assertEqual(len(x_train), train_count)
                self.assertEqual(len(y_train), train_count)

                # check stratified sampling
                counts = y_train.value_counts()
                ratio_train = counts[0]/counts[1]
                counts = y_test.value_counts()
                ratio_test = counts[0]/counts[1]
                logging.debug('nonmatch-match-ratios train=%s, test=%s', ratio_train, ratio_test)
                self.assertAlmostEqual(ratio_train, ratio_test, places=1)

            logging.debug('columns before=%s', [ str(c) for c in df_candidate_pairs.columns] )

            x_train_2, x_test_2, y_train_2, y_test_2 = ontomatch.utils.util.split_df(
                df_candidate_pairs, df_sim_vecs, columns_x=columns_x, column_ml_phase=column_ml_phase)
            if train_size > 0:
                self.assertEqual(len(x_train_2), len(x_train))
                self.assertEqual(len(y_train_2), len(y_train))
            self.assertEqual(len(x_test_2), len(x_test))
            self.assertEqual(len(y_test_2), len(y_test))

            self.assertTrue(isinstance(y_train_2, pd.Series))
            self.assertTrue(isinstance(y_test_2, pd.Series))

        df_candidate_pairs[columns_for_saving].to_csv('../tmp/kg_splits.csv')
