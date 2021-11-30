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
