import logging

import ontomatch.evaluate
import ontomatch.instancematching
import ontomatch.scoring
import tests.utils_for_testing

class TestInstanceMatching(tests.utils_for_testing.TestCaseOntoMatch):

    def get_params_blocking(self):
        return {
            "name": "TokenBasedPairIterator",
            "model_specific": {
                 "min_token_length": 3,
                 "max_token_occurrences_src": 20,
                 "max_token_occurrences_tgt": 20,
                 "blocking_properties": ["name", "isOwnedBy/hasName", "address/addressLocality", "address/streetAddress", "address/postalCode"],
                 "reset_index": False,
            }
        }

    def get_params_model_specific(self):
        return {
            'symmetric': True,
            'delta': 0.05
        }

    #TODO-AE replace method by config file
    def get_prop_prop_sim_tuples_without_geo_coordinates(self):
        params_sim_fcts = [{
                    "name": "dist_nltk_edit",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 3
                },{
                    "name": "dist_absolute",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 10
                },{
                    "name": "dist_relative",
                    "cut_off_mode": "fixed"
                },{
                    "name": "dist_equal",
                    "cut_off_mode": "fixed"
                },{
                    "name": "dist_cosine_with_tfidf",
                    "cut_off_mode": "fixed"
            }
        ]
        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)
        return [
            ('name', 'name', sim_fcts[4]),
            ('isOwnedBy/hasName', 'isOwnedBy/hasName', sim_fcts[0]),
            ('hasYearOfBuilt/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', sim_fcts[1]),
            ('designCapacity/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', sim_fcts[2]),
            #switched from type to fuel
            #('type', 'type', sim_fcts[3]),
            ('realizes/consumesPrimaryFuel', 'realizes/consumesPrimaryFuel', sim_fcts[3]),
        ]

    def get_prop_prop_sim_tuples_with_geo_coordinates(self):
        params_sim_fcts = [{
                    "name": "dist_nltk_edit",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 3
                },{
                    "name": "dist_absolute",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 10
                },{
                    "name": "dist_relative",
                    "cut_off_mode": "fixed"
                },{
                    "name": "dist_equal",
                    "cut_off_mode": "fixed"
                },{
                    "name": "dist_cosine_with_tfidf",
                    "cut_off_mode": "fixed"
            }
        ]
        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)
        return [
            ('name', 'name', sim_fcts[4]),
            ('isOwnedBy/hasName', 'isOwnedBy/hasName', sim_fcts[0]),
            ('hasYearOfBuilt/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', sim_fcts[1]),
            ('designCapacity/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', sim_fcts[2]),
            ('realizes/consumesPrimaryFuel', 'realizes/consumesPrimaryFuel', sim_fcts[3]),
            #TODO-AE long = x ???
            ('geo:wgs84_pos#long', 'hasGISCoordinateSystem/hasProjectedCoordinate_x/hasValue/numericalValue', sim_fcts[2]),
            ('geo:wgs84_pos#lat', 'hasGISCoordinateSystem/hasProjectedCoordinate_y/hasValue/numericalValue', sim_fcts[2])
        ]

    def read_kwl_gppd_DEU_matching_file(self):
        matchfile = tests.utils_for_testing.PATH_MATCHES_PP_DEU
        index_set_matches = ontomatch.evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
        logging.info('ground truth matches=%s', len(index_set_matches))
        return index_set_matches

    def test_auto_calibration_without_geo_coordinates(self):

        params_model_specific = self.get_params_model_specific()
        params_blocking = self.get_params_blocking()
        prop_prop_sim_tuples = self.get_prop_prop_sim_tuples_without_geo_coordinates()

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()

        matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()

        df_total_scores, df_total_best_scores = matcher.start_internal(src_onto, tgt_onto, params_model_specific, params_blocking, prop_prop_sim_tuples=prop_prop_sim_tuples)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        index_set_matches = self.read_kwl_gppd_DEU_matching_file()
        logging.info('length of total best scores=%s', len(df_total_best_scores))
        result = ontomatch.evaluate.evaluate(df_total_best_scores, index_set_matches, number_of_thresholds=11)

        self.assertEqual(len(df_total_best_scores), 1357)

        # max f1-score=0.8874388254486133 for threshold t=0.29999999999999993
        # area under curve=0.9052648606935376
        '''
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.7, 1.0, 0.017241379310344827, 16, 0, 912, 0.03389830508474576],
            [0.6, 1.0, 0.30064655172413796, 279, 0, 649, 0.46230323115161565],
            [0.5, 0.9870967741935484, 0.49461206896551724, 459, 6, 469, 0.6590093323761665],
            [0.3999999999999999, 0.9717514124293786, 0.7413793103448276, 688, 20, 240, 0.8410757946210269],
            [0.29999999999999993, 0.8957189901207464, 0.8793103448275862, 816, 95, 112, 0.8874388254486133],
            [0.19999999999999996, 0.7930720145852325, 0.9375, 870, 227, 58, 0.8592592592592592],
            [0.09999999999999998, 0.6798756798756799, 0.9428879310344828, 875, 412, 53, 0.7900677200902935],
            [0.0, 0.6741140215716487, 0.9428879310344828, 875, 423, 53, 0.7861635220125786]]
        '''

        # max f1-score=0.8854961832061069 for threshold t=0.29999999999999993
        # area under curve=0.9097390253868335
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.7, 1.0, 0.014008620689655173, 13, 0, 915, 0.027630180658873536],
            [0.6, 0.9965635738831615, 0.3125, 290, 1, 638, 0.47579983593109104],
            [0.5, 0.9848484848484849, 0.49030172413793105, 455, 7, 473, 0.6546762589928058],
            [0.3999999999999999, 0.975397973950796, 0.7262931034482759, 674, 17, 254, 0.8326127239036443],
            [0.29999999999999993, 0.8962472406181016, 0.875, 812, 94, 116, 0.8854961832061069],
            [0.19999999999999996, 0.7871956717763751, 0.9407327586206896, 873, 236, 55, 0.8571428571428571],
            [0.09999999999999998, 0.6725057121096725, 0.9515086206896551, 883, 430, 45, 0.7880410531012941],
            [0.0, 0.6514369933677229, 0.9525862068965517, 884, 473, 44, 0.773741794310722]]

        for i, expected in enumerate(expected_result):
            actual = result[i]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)

    def test_auto_calibration_with_geo_coordinates(self):

        params_model_specific = self.get_params_model_specific()
        params_blocking = self.get_params_blocking()
        prop_prop_sim_tuples = self.get_prop_prop_sim_tuples_with_geo_coordinates()

        src_onto, tgt_onto = self.load_kwl_with_geo_coordinates_gppd_ontologies()

        matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()

        df_total_scores, df_total_best_scores = matcher.start_internal(src_onto, tgt_onto, params_model_specific, params_blocking, prop_prop_sim_tuples=prop_prop_sim_tuples)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        index_set_matches = self.read_kwl_gppd_DEU_matching_file()
        logging.info('length of total best scores=%s', len(df_total_best_scores))
        result = ontomatch.evaluate.evaluate(df_total_best_scores, index_set_matches, number_of_thresholds=11)

        self.assertEqual(len(df_total_best_scores), 1349)

        # max f1-score=0.9031903190319032 for threshold t=0.29999999999999993
        # area under curve=0.9093646330565498
        '''
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.7, 1.0, 0.00646551724137931, 6, 0, 922, 0.012847965738758032],
            [0.6, 1.0, 0.07758620689655173, 72, 0, 856, 0.144],
            [0.5, 0.9940298507462687, 0.3588362068965517, 333, 2, 595, 0.5273159144893111],
            [0.3999999999999999, 0.9816232771822359, 0.6907327586206896, 641, 12, 287, 0.8108791903858317],
            [0.29999999999999993, 0.9224719101123595, 0.884698275862069, 821, 69, 107, 0.9031903190319032],
            [0.19999999999999996, 0.7747349823321554, 0.9450431034482759, 877, 255, 51, 0.8514563106796116],
            [0.09999999999999998, 0.678516228748068, 0.9461206896551724, 878, 416, 50, 0.7902790279027904],
            [0.0, 0.6764252696456087, 0.9461206896551724, 878, 420, 50, 0.788858939802336]]
        '''

        # max f1-score=0.9008810572687224 for threshold t=0.29999999999999993
        # area under curve=0.9141845025939161
        expected_result = [[1.0, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.9, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.8, 1.0, 0.0, 0, 0, 928, 0.0],
            [0.7, 1.0, 0.00646551724137931, 6, 0, 922, 0.012847965738758032],
            [0.6, 1.0, 0.07435344827586207, 69, 0, 859, 0.13841524573721165],
            [0.5, 0.9913294797687862, 0.36961206896551724, 343, 3, 585, 0.5384615384615384],
            [0.3999999999999999, 0.9859154929577465, 0.6788793103448276, 630, 9, 298, 0.8040842373962988],
            [0.29999999999999993, 0.9211711711711712, 0.8814655172413793, 818, 70, 110, 0.9008810572687224],
            [0.19999999999999996, 0.7697022767075307, 0.947198275862069, 879, 263, 49, 0.8492753623188405],
            [0.09999999999999998, 0.664167916041979, 0.9547413793103449, 886, 448, 42, 0.7833775419982316],
            [0.0, 0.6567828020756116, 0.9547413793103449, 886, 463, 42, 0.7782169521299956]]

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

        params = self.read_conf_kwl()
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
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

        # max f1-score=0.8181268882175227 for threshold t=0.9
        # area under curve=0.8232895636412483
        '''
        expected_result = [[1.0, 0.9941002949852508, 0.36314655172413796, 337, 2, 591, 0.531965272296764],
            [0.9, 0.9312242090784044, 0.7295258620689655, 677, 50, 251, 0.8181268882175227],
            [0.8, 0.7903402854006586, 0.7758620689655172, 720, 191, 208, 0.7830342577487764],
            [0.7, 0.7626943005181347, 0.7931034482758621, 736, 229, 192, 0.7776016904384575],
            [0.6, 0.7350928641251222, 0.8103448275862069, 752, 271, 176, 0.7708867247565351],
            [0.5, 0.6988950276243094, 0.8178879310344828, 759, 327, 169, 0.7537239324726912],
            [0.3999999999999999, 0.6507258753202391, 0.8211206896551724, 762, 409, 166, 0.7260600285850404],
            [0.29999999999999993, 0.5889145496535797, 0.8243534482758621, 765, 534, 163, 0.6870229007633588],
            [0.19999999999999996, 0.5072463768115942, 0.8674568965517241, 805, 782, 123, 0.6401590457256461],
            [0.09999999999999998, 0.26840215439856374, 0.9665948275862069, 897, 2445, 31, 0.4201405152224824],
            [0.0, 0.16828358208955224, 0.9719827586206896, 902, 4458, 26, 0.2868956743002545]]
        '''

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
