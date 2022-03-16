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

        self.assertEqual(len(df_total_best_scores), 1401)

        # max f1=0.87948 for t=0.3, p=0.88621, r=0.87284, area under curve=0.9058249439000001
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.7, 1.0, 0.00647, 6, 0, 922, 0.01285],
            [0.6, 0.99652, 0.30819, 286, 1, 642, 0.47078],
            [0.5, 0.98532, 0.50647, 470, 7, 458, 0.66904],
            [0.4, 0.96987, 0.72845, 676, 21, 252, 0.832],
            [0.3, 0.88621, 0.87284, 810, 104, 118, 0.87948],
            [0.2, 0.7547, 0.95151, 883, 287, 45, 0.84175],
            [0.1, 0.64058, 0.95259, 884, 496, 44, 0.76603],
            [0.0, 0.63098, 0.95259, 884, 517, 44, 0.75912]]

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

        self.assertEqual(len(df_total_best_scores), 1347)

        # max f1=0.91123 for t=0.3, p=0.90446, r=0.9181, area under curve=0.9139892730000001
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.7, 1.0, 0.00323, 3, 0, 925, 0.00644],
            [0.6, 1.0, 0.07651, 71, 0, 857, 0.14214],
            [0.5, 0.99198, 0.39978, 371, 3, 557, 0.56989],
            [0.4, 0.97765, 0.7069, 656, 15, 272, 0.82051],
            [0.3, 0.90446, 0.9181, 852, 90, 76, 0.91123],
            [0.2, 0.74264, 0.95151, 883, 306, 45, 0.8342],
            [0.1, 0.6597, 0.95259, 884, 456, 44, 0.77954],
            [0.0, 0.65627, 0.95259, 884, 463, 44, 0.77714]]

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

        # max f1-score=0.8097671777399206 for threshold t=0.8
        # area under curve=0.8316915503444298
        expected_result = [[1.0, 0.9968847352024922, 0.3448275862068966, 320, 1, 608, 0.512409927942354],
            [0.9, 0.9521674140508222, 0.6864224137931034, 637, 32, 291, 0.7977457733249844],
            [0.8, 0.8559423769507803, 0.7683189655172413, 713, 120, 215, 0.8097671777399206],
            [0.7, 0.8019271948608137, 0.8071120689655172, 749, 185, 179, 0.8045112781954887],
            [0.6, 0.7432170542635659, 0.8265086206896551, 767, 265, 161, 0.7826530612244899],
            [0.5, 0.6701208981001727, 0.8362068965517241, 776, 382, 152, 0.7440076701821667],
            [0.3999999999999999, 0.595256312165264, 0.8383620689655172, 778, 529, 150, 0.6961968680089485],
            [0.29999999999999993, 0.5022508038585209, 0.8415948275862069, 781, 774, 147, 0.6290777285541683],
            [0.19999999999999996, 0.4026812313803376, 0.8739224137931034, 811, 1203, 117, 0.5513256288239293],
            [0.09999999999999998, 0.2530933633295838, 0.9698275862068966, 900, 2656, 28, 0.4014272970561999],
            [0.0, 0.16828358208955224, 0.9719827586206896, 902, 4458, 26, 0.2868956743002545]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)
