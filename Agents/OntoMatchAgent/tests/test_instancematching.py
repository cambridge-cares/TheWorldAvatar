import logging

import ontomatch.evaluate
import ontomatch.instancematching
import ontomatch.scoring
import tests.utils_for_testing

class TestInstanceMatching(tests.utils_for_testing.TestCaseOntoMatch):

    def get_params_model_specific(self):
        return {
            'symmetric': True,
            'delta': 0.05
        }

    def test_auto_calibration_without_geo_coordinates(self):

        params_model_specific = self.get_params_model_specific()

        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_AUTO)
        params_blocking = params['blocking']
        params_mapping = params['mapping']

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()

        matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()

        _, df_total_best_scores = matcher.start_internal(src_onto, tgt_onto, params_model_specific, params_blocking, params_mapping=params_mapping)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        index_set_matches = self.read_kwl_gppd_DEU_matching_file()
        logging.info('length of total best scores=%s', len(df_total_best_scores))
        result = ontomatch.evaluate.evaluate(df_total_best_scores, index_set_matches, number_of_thresholds=11)

        self.assertEqual(len(df_total_best_scores), 1412)

        # evaluation result - maximum: t=0.3, f1=0.87076, p=0.93235, r=0.81681
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0], 
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0], 
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0], 
            [0.7, 1.0, 0.0, 0, 0, 928, 0.0], 
            [0.6, 0.99603, 0.27047, 251, 1, 677, 0.42542], 
            [0.5, 0.99752, 0.43427, 403, 1, 525, 0.60511], 
            [0.4, 0.98696, 0.57112, 530, 7, 398, 0.72355], 
            [0.3, 0.93235, 0.81681, 758, 55, 170, 0.87076], 
            [0.2, 0.80309, 0.89655, 832, 204, 96, 0.84725], 
            [0.1, 0.68124, 0.92349, 857, 401, 71, 0.78408], 
            [0.0, 0.62535, 0.95151, 883, 529, 45, 0.7547]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)

    def test_auto_calibration_with_geo_coordinates(self):

        params_model_specific = self.get_params_model_specific()

        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_AUTO_GEO)
        params_blocking = params['blocking']
        params_mapping = params['mapping']

        src_onto, tgt_onto = self.load_kwl_with_geo_coordinates_gppd_ontologies()

        matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()
        _, df_total_best_scores = matcher.start_internal(src_onto, tgt_onto, params_model_specific, params_blocking, params_mapping=params_mapping)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        index_set_matches = self.read_kwl_gppd_DEU_matching_file()
        logging.info('length of total best scores=%s', len(df_total_best_scores))
        result = ontomatch.evaluate.evaluate(df_total_best_scores, index_set_matches, number_of_thresholds=11)

        self.assertEqual(len(df_total_best_scores), 1359)

        # evaluation result - maximum: t=0.2, f1=0.85862, p=0.82376, r=0.89655
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0], 
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0], 
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0], 
            [0.7, 1.0, 0.0, 0, 0, 928, 0.0], 
            [0.6, 1.0, 0.00216, 2, 0, 926, 0.0043], 
            [0.5, 0.996, 0.26832, 249, 1, 679, 0.42275], 
            [0.4, 0.9938, 0.51832, 481, 3, 447, 0.6813], 
            [0.3, 0.95716, 0.77047, 715, 32, 213, 0.85373], 
            [0.2, 0.82376, 0.89655, 832, 178, 96, 0.85862], 
            [0.1, 0.67699, 0.94181, 874, 417, 54, 0.78774], 
            [0.0, 0.65195, 0.95474, 886, 473, 42, 0.77481]]

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

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_WEIGHT)
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

        matches = self.read_kwl_gppd_DEU_matching_file()
        result = ontomatch.evaluate.evaluate(scores, matches, number_of_thresholds=11)

        # evaluation result - maximum: t=0.8, f1=0.80754, p=0.85905, r=0.76185
        # area under curve=0.8316915503444298
        expected_result = [[1.0, 0.99688, 0.34483, 320, 1, 608, 0.51241], 
            [0.9, 0.95188, 0.68211, 633, 32, 295, 0.79473], 
            [0.8, 0.85905, 0.76185, 707, 116, 221, 0.80754], 
            [0.7, 0.80498, 0.80065, 743, 180, 185, 0.80281], 
            [0.6, 0.74656, 0.81897, 760, 258, 168, 0.78109], 
            [0.5, 0.67483, 0.8319, 772, 372, 156, 0.74517], 
            [0.4, 0.6, 0.83405, 774, 516, 154, 0.69793], 
            [0.3, 0.50751, 0.83728, 777, 754, 151, 0.63196], 
            [0.2, 0.40553, 0.86961, 807, 1183, 121, 0.55312], 
            [0.1, 0.25388, 0.96983, 900, 2645, 28, 0.40241], 
            [0.0, 0.16828, 0.97198, 902, 4458, 26, 0.2869]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)

    def test_auto_calibration_restaurant(self):

        params = self.read_params(tests.utils_for_testing.PATH_CONF_REST_AUTO_CSV)
        params_blocking = params['blocking']
        params_mapping = params['mapping']
        #params_model_specific = self.get_params_model_specific()
        params_model_specific = params['matching']['model_specific']

        src_onto, tgt_onto = self.read_restaurant_tables()

        matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()

        _, df_total_best_scores = matcher.start_internal(src_onto, tgt_onto, params_model_specific, params_blocking, params_mapping=params_mapping)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        index_set_matches = self.read_restaurant_matching_file()
        logging.info('length of total best scores=%s', len(df_total_best_scores))
        result = ontomatch.evaluate.evaluate(df_total_best_scores, index_set_matches, number_of_thresholds=11)

        print('RESULT', result)

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