import logging

import ontomatch.evaluate
import ontomatch.instancematching
import ontomatch.scoring
import tests.utils_for_testing

class TestInstanceMatching(tests.utils_for_testing.TestCaseOntoMatch):

    def get_params_model_specific(self):
        return {
            'delta': 0.025
        }

    def test_auto_calibration_without_geo_coordinates(self):

        params_model_specific = self.get_params_model_specific()

        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_AUTO_CSV)
        params_blocking = params['blocking']
        params_mapping = params['mapping']

        src_onto, tgt_onto = self.read_kwl_gppd_tables()

        matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()

        _, df_total_best_scores = matcher.start_internal(src_onto, tgt_onto, params_model_specific, params_blocking, params_mapping=params_mapping)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        index_set_matches = self.read_klw_gppd_DEU_matches()
        logging.info('length of total best scores=%s', len(df_total_best_scores))
        result = ontomatch.evaluate.evaluate(df_total_best_scores, index_set_matches, number_of_thresholds=11)

        self.assertEqual(len(df_total_best_scores), 1228)

        # evaluation result - maximum: t=0.3, f1=0.87836, p=0.93292, r=0.82983
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 905, 0.0], 
            [0.9, 1.0, 0.0, 0, 0, 905, 0.0], 
            [0.8, 1.0, 0.0, 0, 0, 905, 0.0], 
            [0.7, 1.0, 0.00552, 5, 0, 900, 0.01099], 
            [0.6, 0.99645, 0.3105, 281, 1, 624, 0.47346], 
            [0.5, 0.99775, 0.49061, 444, 1, 461, 0.65778], 
            [0.4, 0.98311, 0.57901, 524, 9, 381, 0.72879], 
            [0.3, 0.93292, 0.82983, 751, 54, 154, 0.87836], 
            [0.2, 0.81791, 0.90829, 822, 183, 83, 0.86073], 
            [0.1, 0.71688, 0.91492, 828, 327, 77, 0.80388], 
            [0.0, 0.67915, 0.92155, 834, 394, 71, 0.782]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)

    def test_get_total_score_for_row(self):
        prop_scores = [[1., 0.5, None, 0., 0.5], [None, 0.7, None, 0.3]]
        scoring_weights = [[1., 0., 1., 1., 0.1], [1., 0.5, 0.5, 0.5]]
        expected = [[2., 1.05, 2.4, 2.05, 3/5, 2/4], [1., 0.5, 1.8, 2., 3/4, 0.]]

        for i, scores in enumerate(prop_scores):
            logging.debug('iteration i=%s', i)
            total_score = ontomatch.instancematching.get_total_score_for_row(scores)
            self.assertEqual(total_score, expected[i][0])
            total_score = ontomatch.instancematching.get_total_score_for_row(scores, scoring_weights[i])
            self.assertEqual(total_score, expected[i][1])
            total_score = ontomatch.instancematching.get_total_score_for_row(scores, missing_score=0.4)
            self.assertEqual(total_score, expected[i][2])
            total_score = ontomatch.instancematching.get_total_score_for_row(scores, scoring_weights[i], missing_score=1.)
            self.assertEqual(total_score, expected[i][3])
            total_score = ontomatch.instancematching.get_total_score_for_row(scores, missing_score=1., aggregation_mode='mean', average_min_prop_count=1)
            self.assertEqual(total_score, expected[i][4])
            total_score = ontomatch.instancematching.get_total_score_for_row(scores, aggregation_mode='mean', average_min_prop_count=3)
            self.assertEqual(total_score, expected[i][5])

    def test_matching_with_scoring_weights_without_geo_coordinates(self):

        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_WEIGHT_CSV)
        params_blocking = params['blocking']
        params_mapping = params['mapping']
        params_post_processing = params['post_processing']
        scoring_weights = params['matching']['model_specific']['weights']

        matcher = ontomatch.instancematching.InstanceMatcherWithScoringWeights()
        matcher.start_internal(src_onto, tgt_onto, params_blocking, scoring_weights, params_post_processing, params_mapping)
        scores = matcher.get_scores()
        logging.debug('number=%s', len(scores))
        logging.debug('columns=%s', [ str(c) for c in scores.columns])

        weight_sum = sum(scoring_weights)
        scoring_weights = [ w/weight_sum for w in scoring_weights]

        ontomatch.instancematching.add_total_scores(scores, props=[0, 1, 2, 3, 4], scoring_weights=scoring_weights)

        matches = self.read_klw_gppd_DEU_matches()
        result = ontomatch.evaluate.evaluate(scores, matches, number_of_thresholds=11)

        #evaluation result - maximum: t=0.8, f1=0.82093, p=0.86626, r=0.78011
        expected_result = [[1.0, 0.99688, 0.35359, 320, 1, 585, 0.52202], 
            [0.9, 0.95173, 0.69724, 631, 32, 274, 0.80485], 
            [0.8, 0.86626, 0.78011, 706, 109, 199, 0.82093], 
            [0.7, 0.81421, 0.8232, 745, 170, 160, 0.81868], 
            [0.6, 0.7552, 0.84199, 762, 247, 143, 0.79624], 
            [0.5, 0.68402, 0.85635, 775, 358, 130, 0.76055], 
            [0.4, 0.61037, 0.85856, 777, 496, 128, 0.7135], 
            [0.3, 0.5196, 0.86409, 782, 723, 123, 0.64896], 
            [0.2, 0.41517, 0.89503, 810, 1141, 95, 0.56723], 
            [0.1, 0.24934, 0.94144, 852, 2565, 53, 0.39426], 
            [0.0, 0.18097, 0.94365, 854, 3865, 51, 0.3037]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)

    def test_auto_calibration_restaurant(self):

        params = self.read_params(tests.utils_for_testing.PATH_CONF_REST_AUTO_CSV)
        params_blocking = params['blocking']
        params_mapping = params['mapping']
        params_model_specific = params['matching']['model_specific']

        src_onto, tgt_onto = self.read_restaurant_tables()

        matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()

        _, df_total_best_scores = matcher.start_internal(src_onto, tgt_onto, params_model_specific, params_blocking, params_mapping=params_mapping)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        index_set_matches = self.read_restaurant_matches()
        logging.info('length of total best scores=%s', len(df_total_best_scores))
        result = ontomatch.evaluate.evaluate(df_total_best_scores, index_set_matches, number_of_thresholds=11)

        self.assertEqual(len(df_total_best_scores), 659)

        # evaluation result - maximum: t=0.4, f1=0.95154, p=0.92308, r=0.98182
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 110, 0.0], 
            [0.9, 1.0, 0.0, 0, 0, 110, 0.0], 
            [0.8, 1.0, 0.00909, 1, 0, 109, 0.01802], 
            [0.7, 1.0, 0.2, 22, 0, 88, 0.33333], 
            [0.6, 1.0, 0.6, 66, 0, 44, 0.75], 
            [0.5, 0.9901, 0.90909, 100, 1, 10, 0.94787], 
            [0.4, 0.92308, 0.98182, 108, 9, 2, 0.95154], 
            [0.3, 0.72368, 1.0, 110, 42, 0, 0.83969], 
            [0.2, 0.27848, 1.0, 110, 285, 0, 0.43564], 
            [0.1, 0.1682, 1.0, 110, 544, 0, 0.28796], 
            [0.0, 0.16692, 1.0, 110, 549, 0, 0.28609]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)