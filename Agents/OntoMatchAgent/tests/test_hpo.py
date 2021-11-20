import logging

import pandas as pd

import ontomatch.classification
import ontomatch.hpo
import tests.utils_for_testing

class TestHPO(tests.utils_for_testing.TestCaseOntoMatch):

    def xxx_test_start_RF_total_scores(self):
        params_classification = {
            "name": "RF",
            "model_specific":{
                "bootstrap": True, "max_depth": 50, "max_features": 0.5, "max_samples": 50, "min_samples_leaf": 1, "min_samples_split": 2, "n_estimators": 128
            },
            "cross_validation": 5
        }
        path = 'C:/my/repos/ontomatch_20210924/experiments/211115_visualization/KWL/scores_1/total_scores.csv'
        dframe = pd.read_csv(path, index_col=['idx_1', 'idx_2'])
        fct_to_int = lambda b: 1 if b else 0
        y_column = 'y'
        dframe[y_column] = dframe['best'].apply(fct_to_int)

        x_columns = ['0', '1', '2', '3', '4']
        train_size = 0.8
        result = ontomatch.hpo.split_hpo_evaluate(dframe, params_classification,
                x_columns=x_columns, y_column=y_column, train_size=train_size)

    def xxx_test_start_RF_scores(self):
        params_classification = {
            "name": "RF",
            "model_specific":{
                "bootstrap": True, "max_depth": 50, "max_features": 0.5, "max_samples": 50, "min_samples_leaf": 1, "min_samples_split": 2, "n_estimators": 128
            },
            "cross_validation": 5
        }
        path = 'C:/my/repos/ontomatch_20210924/experiments/211115_visualization/KWL/scores_1/scores.csv'
        dframe = pd.read_csv(path, index_col=['idx_1', 'idx_2'])

        # mark true matches in the scores data
        df_matches = pd.read_csv(tests.utils_for_testing.PATH_MATCHES_PP_DEU, index_col=['idx_1', 'idx_2'])
        index_intersection = dframe.index.intersection(df_matches.index)
        logging.info('df_matches=%s, intersection with scores=%s', len(df_matches), len(index_intersection))
        y_column = 'y'
        dframe[y_column] = 0
        dframe.at[index_intersection, y_column] = 1

        x_columns = ['0', '1', '2', '3', '4']
        train_size = 0.02
        result = ontomatch.hpo.split_hpo_evaluate(dframe, params_classification,
                x_columns=x_columns, y_column=y_column, train_size=train_size)

    def test_start_XGB_scores(self):
        params_classification = {
            "name": "XGB",
            "model_specific":{
                "n_estimators": [32, 64],
		        "max_depth": [2,4],
		        "learning_rate": [0.05, 0.3],
		        "scale_pos_weight": None
            },
            "cross_validation": 5
        }
        match_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_DEU_M_ground_truth_tfidf.csv'
        nonmatch_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_DEU_N_random_blocking_tfidf_ratio_1.csv'
        train_size = 0.2
        column_ml_phase = 'ml_phase_' + str(train_size)
        prop_columns=['0', '1', '2', '3', '4']
        x_train, x_test, y_train, y_test = ontomatch.classification.TrainTestGenerator.train_test_split(
                match_file, nonmatch_file, column_ml_phase, prop_columns)

        model = ontomatch.hpo.start_hpo(params_classification, x_train, y_train)
        result = ontomatch.hpo.evaluate_with_pred_proba(model, x_test, y_test, 11)

        # max f1-score=0.864373783257625 for threshold t=0.7
        # area under curve=0.8794392545480988
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 743, 0.0],
            [0.9, 0.9377049180327869, 0.7698519515477793, 572, 38, 171, 0.8455284552845529],
            [0.8, 0.8792134831460674, 0.8425302826379543, 626, 86, 117, 0.8604810996563574],
            [0.7, 0.8345864661654135, 0.8963660834454913, 666, 132, 77, 0.864373783257625],
            [0.6, 0.8059880239520958, 0.9057873485868102, 673, 162, 70, 0.85297845373891],
            [0.5, 0.723336853220697, 0.9219380888290714, 685, 262, 58, 0.8106508875739645],
            [0.4, 0.6889332003988036, 0.9300134589502019, 691, 312, 52, 0.7915234822451318],
            [0.3, 0.6625239005736138, 0.9327052489905787, 693, 353, 50, 0.7747344885410845],
            [0.2, 0.5726978998384491, 0.9542395693135935, 709, 529, 34, 0.7158001009591116],
            [0.1, 0.42501481920569056, 0.9650067294751009, 717, 970, 26, 0.5901234567901235],
            [0.0, 0.14812599681020733, 1.0, 743, 4273, 0, 0.25803090814377494]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)
