import logging
import unittest.mock

import pandas as pd

import ontomatch.blocking
import ontomatch.classification
import ontomatch.evaluate
import ontomatch.utils.util
import tests.utils_for_testing

class TestClassification(tests.utils_for_testing.TestCaseOntoMatch):

    def test_create_train_test_split(self):
        path = 'C:/my/repos/ontomatch_20210924/experiments/211115_visualization/KWL/scores_1/total_scores.csv'
        dframe = ontomatch.utils.util.read_csv(path)
        len_total = len(dframe)
        y = 'best'
        mask = (dframe[y] == True)
        len_best = len(dframe[mask])
        train_size = 0.2
        df_split = ontomatch.classification.Utils.create_train_test_split(dframe=path, train_size=train_size, stratify_y_column=y, save_to=None)

        logging.debug('columns=%s', df_split.columns)

        len_split = len(df_split)

        mask = ((df_split['ml_phase'] == 'train') & (df_split[y] == True))
        len_0 = len(df_split[mask])
        mask = ((df_split['ml_phase'] == 'test') & (df_split[y] == True))
        len_1 = len(df_split[mask])
        mask = ((df_split['ml_phase'] == 'train') & (df_split[y] == False))
        len_2 = len(df_split[mask])
        mask = ((df_split['ml_phase'] == 'test') & (df_split[y] == False))
        len_3 = len(df_split[mask])

        logging.debug('%s, %s, %s %s %s %s %s', len_total, len_best, len_split, len_0, len_1, len_2, len_3)
        # check split due to train_size
        self.assertAlmostEquals(len_0 + len_2, train_size * len_total, places=1)
        # check stratified split due to y
        self.assertAlmostEquals(len_0 + len_1, len_best)

    def xxx_test_train_SVC_with_seeds_for_ground_truth(self):

        testargs = ['test',
            '--config', tests.utils_for_testing.PATH_CONF_PP_DEU_AUTO_GEO
        ]

        with unittest.mock.patch('sys.argv', testargs):
            params, _ = ontomatch.utils.util.init()

            df1, df2 = ontomatch.classification.Utils.create_dataframes(params)

            matchfile = tests.utils_for_testing.PATH_MATCHES_PP_DEU
            match_index = ontomatch.evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])

            params_mapping = params['mapping']

            df_train, df_test, labels_train, labels_test = ontomatch.classification.select_seeds_for_ground_truth(
                df1, df2, match_index, params_mapping, split=[.8, .2], missing_value=None)

            params_hpo = [{
                'kernel': ['rbf'],
                'gamma': ['scale'],
                'C': [1.0],
            }]

            model  = ontomatch.classification.hpo_svm(df_train, labels_train, params_hpo, crossvalidation=5)
            train_score = model.score(df_train, labels_train)
            test_score = model.score(df_test, labels_test)
            logging.debug('SVC train_score=%s, test_score=%s', train_score, test_score)
            # TODO-AE: scores are random
            # SVC train_score=0.579610538373425, test_score=0.54337899543379
            self.assertAlmostEqual(train_score, 0.58648, places=2)
            self.assertAlmostEqual(test_score, 0.53424, places=2)

    def xxx_test_train_SVC_with_scores_kwl_auto(self):

        directory = './tests/data/scores_kwl_auto'
        columns = ['0','1','2','3','4']

        df_max_scores = pd.read_csv(directory + '/max_scores_1.csv', index_col=['idx_1', 'idx_2'])
        df_scores = pd.read_csv(directory + '/scores.csv', index_col=['idx_1', 'idx_2'])[columns]
        df_train, df_test, labels_train, labels_test = ontomatch.classification.select_seeds_for_max_scores(df_max_scores, df_scores)

        params_hpo = [{
            'kernel': ['rbf'],
            'gamma': ['scale'],
            'C': [1.0],
        }]
        model  = ontomatch.classification.hpo_svm(df_train, labels_train, params_hpo, crossvalidation=5)
        train_score = model.score(df_train, labels_train)
        test_score = model.score(df_test, labels_test)
        logging.debug('SVC train_score=%s, test_score=%s', train_score, test_score)
        # TODO-AE: scores are random
        # SVC train_score=0.8493524199045671, test_score=0.8692098092643051
        # SVC train_score=0.8534423994546694, test_score=0.8256130790190735
        # SVC train_score=0.8513974096796183, test_score=0.8310626702997275
        self.assertAlmostEqual(train_score, 0.85139, places=2)
        self.assertAlmostEqual(test_score, 0.83106, places=2)
