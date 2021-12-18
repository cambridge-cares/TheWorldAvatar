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

        self.assertEqual(len(df_total_best_scores), 1342)

        # 2021-12-15 11:28:26,887 INFO     max f1-score=0.8841530054644808 for threshold t=0.29999999999999993
        # 2021-12-15 11:28:26,888 INFO     area under curve=0.9097632261949141
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.7, 1.0, 0.00646551724137931, 6, 0, 922, 0.012847965738758032],
            [0.6, 0.9965156794425087, 0.3081896551724138, 286, 1, 642, 0.4707818930041152],
            [0.5, 0.9873684210526316, 0.5053879310344828, 469, 6, 459, 0.6685673556664291],
            [0.3999999999999999, 0.9726224783861671, 0.7273706896551724, 675, 19, 253, 0.8323057953144265],
            [0.29999999999999993, 0.8968957871396895, 0.8717672413793104, 809, 93, 119, 0.8841530054644808],
            [0.19999999999999996, 0.79491833030853, 0.9439655172413793, 876, 226, 52, 0.8630541871921182],
            [0.09999999999999998, 0.6776669224865695, 0.9515086206896551, 883, 420, 45, 0.7915732855221874],
            [0.0, 0.6569940476190477, 0.9515086206896551, 883, 461, 45, 0.7772887323943664]]

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

        self.assertEqual(len(df_total_best_scores), 1333)

        # 2021-12-15 11:34:14,313 INFO     max f1-score=0.9166666666666667 for threshold t=0.29999999999999993
        # 2021-12-15 11:34:14,313 INFO     area under curve=0.9165683553240749
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.7, 1.0, 0.003232758620689655, 3, 0, 925, 0.006444683136412459],
            [0.6, 1.0, 0.07650862068965517, 71, 0, 857, 0.14214214214214213],
            [0.5, 0.9919786096256684, 0.39978448275862066, 371, 3, 557, 0.5698924731182795],
            [0.3999999999999999, 0.9820089955022488, 0.7058189655172413, 655, 12, 273, 0.8213166144200627],
            [0.29999999999999993, 0.9206521739130434, 0.9127155172413793, 847, 73, 81, 0.9166666666666667],
            [0.19999999999999996, 0.7628149435273676, 0.9461206896551724, 878, 273, 50, 0.8446368446368446],
            [0.09999999999999998, 0.6697038724373576, 0.9504310344827587, 882, 435, 46, 0.7857461024498886],
            [0.0, 0.6616654163540885, 0.9504310344827587, 882, 451, 46, 0.7801857585139319]]

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
