import logging

import pandas as pd

import ontomatch.evaluate
import ontomatch.hpo
import tests.utils_for_testing

class TestHPO(tests.utils_for_testing.TestCaseOntoMatch):

    def train_test_split_OLD(self, match_file, nonmatch_file, column_ml_phase, prop_columns=None):
        logging.info('splitting, match=%s, nonmatch=%s, ml_phase=%s, columns=%s',
            match_file, nonmatch_file, column_ml_phase, prop_columns)

        keep_columns = prop_columns.copy()
        keep_columns.extend(['y', column_ml_phase])

        df_matches = ontomatch.utils.util.read_csv(match_file)
        df_matches = df_matches[keep_columns].copy()

        df_nonmatches = ontomatch.utils.util.read_csv(nonmatch_file)
        df_nonmatches = df_nonmatches[keep_columns].copy()

        dframe = pd.concat([df_matches, df_nonmatches])
        mask = (dframe[column_ml_phase] == 'train')
        x_train = dframe[mask][prop_columns].copy()
        y_train = dframe[mask]['y'].copy()
        mask = (dframe[column_ml_phase] == 'test')
        x_test = dframe[mask][prop_columns].copy()
        y_test = dframe[mask]['y'].copy()

        logging.info('x_train=%s, y_train=%s, x_test=%s, y_test=%s', len(x_train), len(y_train), len(x_test), len(y_test))
        return x_train, x_test, y_train, y_test

    def test_start_XGB_scores(self):
        params_classification = {
            "name": "XGB",
            "model_specific":{
                "n_estimators": [32, 64],
		        "max_depth": [2,4],
		        "learning_rate": [0.05, 0.3],
		        "scale_pos_weight": None
            }
        }
        params_training = {
	        "match_train_size": 0.2,
	        "nonmatch_ratio": 1,
	        "train_file": None,
	        "cross_validation": 5
        }

        params_impution = {
		    "name": "sklearn.impute.KNNImputer",
		    "model_specific": {
			    "n_neighbors": [5],
			    "weights": ["uniform"]
		    }
        }

        match_file = './tests/data/power_plant_DEU_M_ground_truth_tfidf.csv'
        nonmatch_file = './tests/data/power_plant_DEU_N_random_blocking_tfidf_ratio_1.csv'
        train_size = 0.2
        column_ml_phase = 'ml_phase_' + str(train_size)
        prop_columns=['0', '1', '2', '3', '4']
        x_train, x_test, y_train, y_test = self.train_test_split_OLD(match_file, nonmatch_file, column_ml_phase, prop_columns)

        cross_validation = params_training['cross_validation']
        model = ontomatch.hpo.start_hpo(params_classification, cross_validation, params_impution, x_train, y_train, x_test=None, y_test=None)
        result = ontomatch.evaluate.evaluate_with_pred_proba(model, x_test, y_test, 11)

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
