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

    def test_hpo_XGB_with_NM_ratio(self):
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
	        #"nonmatch_ratio": 1,
	        "train_file": None,
	        "cross_validation": 5
        }

        match_file = './tests/data/power_plant_DEU_M_ground_truth_tfidf.csv'
        nonmatch_file = './tests/data/power_plant_DEU_N_random_blocking_tfidf_ratio_1.csv'
        train_size = 0.2
        column_ml_phase = 'ml_phase_' + str(train_size)
        prop_columns=['0', '1', '2', '3', '4']
        x_train, x_test, y_train, y_test = self.train_test_split_OLD(match_file, nonmatch_file, column_ml_phase, prop_columns)

        cross_validation = params_training['cross_validation']
        model = ontomatch.hpo.start_hpo(params_classification, cross_validation, None, x_train, y_train, x_test=None, y_test=None, unit_test=True)
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

    def test_hpo_MLP_with_stratified_train_test_split(self):
        params_classification = {
            "name": "MLP",
            "model_specific":{
		        "hidden_layer_sizes": ["20", "10,5"],
		        "learning_rate": ["adaptive"],
		        "learning_rate_init": [0.0001],
		        "alpha": [0.0001],
		        "beta_1": [0.9],
		        "beta_2": [0.999]
            }
        }
        params_training = {
	        "match_train_size": 0.1,
	        "train_file": "./conf/power_plant_DEU/split_kg_mtf_200.csv",
	        "cross_validation": 2
        }
        params_impution = {
		    "name": "sklearn.impute.KNNImputer",
		    "model_specific": {
			    "n_neighbors": [5],
			    "weights": ["uniform"]
		    }
        }

        similarity_file = 'C:/my/repos/ontomatch_20210924/preprint/experiments/power_plant_DEU/sim_vectors_kg/sim_vectors_kg_geo_emb.csv'
        df_scores = ontomatch.utils.util.read_csv(similarity_file)
        logging.debug('total scores=%s', len(df_scores))
        prop_columns = ontomatch.utils.util.get_prop_columns(df_scores)
        logging.debug('columns=%s', prop_columns)

        train_file = params_training['train_file']
        df_split = ontomatch.utils.util.read_csv(train_file)
        split_column = 'ml_phase_0.1'
        x_train, x_test, y_train, y_test = ontomatch.utils.util.split_df(df_split, df_scores, columns_x=prop_columns, column_ml_phase=split_column)

        cross_validation = params_training['cross_validation']
        model = ontomatch.hpo.start_hpo(params_classification, cross_validation, params_impution, x_train, y_train, x_test=None, y_test=None, unit_test=True)
        result = ontomatch.evaluate.evaluate_with_pred_proba(model, x_test, y_test, 11)

        # evaluation result: max f1=0.81008 for t=0.3, p=0.82067, r=0.79976, area under curve=0.8216735876000001
        # evaluation result - by threshold 0.5: max f1=0.79869 for t=0.5, p=0.86876, r=0.73908, TP=609, FP=92, FN=215
        # threshold, precision, recall, TP, FP, FN, f1score:
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 824, 0.0],
            [0.9, 0.98319, 0.42597, 351, 6, 473, 0.59441],
            [0.8, 0.94171, 0.62743, 517, 32, 307, 0.7531],
            [0.7, 0.90161, 0.6784, 559, 61, 265, 0.77424],
            [0.6, 0.88218, 0.70874, 584, 78, 240, 0.786],
            [0.5, 0.86876, 0.73908, 609, 92, 215, 0.79869],
            [0.4, 0.84091, 0.76335, 629, 119, 195, 0.80025],
            [0.3, 0.82067, 0.79976, 659, 144, 165, 0.81008],
            [0.2, 0.76075, 0.83738, 690, 217, 134, 0.79723],
            [0.1, 0.6393, 0.88835, 732, 413, 92, 0.74352],
            [0.0, 0.02306, 1.0, 824, 34906, 0, 0.04508]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)
