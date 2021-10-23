import logging
import pickle

from alignment import Alignment
import coordinator
import evaluate
import scoring
import utils_for_testing

class TestCoordinator(utils_for_testing.TestCaseOntoMatch):

    def get_default_params(self, srcaddr, tgtaddr):
        return {
            "dataset": {
                "src": srcaddr,
                "tgt": tgtaddr,
            },
            "pre_processing": {
                "add_knowledge": "knowledge.geocoding",
                "pickle_dump": False,
            },
            "blocking": {
                #"name": "FullPairIterator",
                "name": "TokenBasedPairIterator",
                "model_specific": {
                    "min_token_length": 3,
                    "max_token_occurrences_src": 20,
                    "max_token_occurrences_tgt": 20,
                    "blocking_properties": ["name", "isOwnedBy/hasName"],
                    "reset_index": False,
                }
            },
            "mapping": {
            },
            "matching": {
                "name": "matchManager.matchManager",
                "model_specific": {
                    "steps": ["ValueMatcher", "instanceStringMatcher", "instanceBOWMatcher"],
                    "weights": [0.5, 0.4, 0.1],
                    "params": [None, None, None],
                    "threshold": 0.2,
                },
            }
        }

    def get_params_for_property_mapping_with_auto_calibration(self, srcaddr, tgtaddr):
        params = self.get_default_params(srcaddr, tgtaddr)

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
                }
        ]

        params['mapping'] =  {
                "mode": "auto",
                "similarity_functions": params_sim_fcts
        }

        params['matching'] = {
            "name": "coordinator.InstanceMatcherWithAutoCalibration",
            "model_specific": {
            }
        }
        return params

    def test_coordinator_load_add_knowledge(self):

        srcaddr = './tests/data/KWL_20_power_plants.ttl'

        agent = coordinator.Agent()
        graph = agent.load_rdflib_graph(srcaddr, add_knowledge="knowledge.geocoding")

        query = '''
        PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
        SELECT ?subj ?lat ?long
        WHERE {
            ?subj geo:lat ?lat .
            ?subj geo:long ?long .
        }'''

        result = list(graph.query(query))
        self.assertEqual(len(result), 18)

        onto = agent.load_owlready2_ontology(graph)
        self.assertIsNotNone(onto)

    def test_coordinator_load_add_knowledge_dump(self):
        srcaddr = './data/kwl_address_geo_211022.ttl'
        tgtaddr = './data/gppd_DEU_geo_211022.ttl'

        agent = coordinator.Agent()
        agent.load(srcaddr, tgtaddr, add_knowledge="knowledge.geocoding", dump_ontology=True)

    def test_coordinator_start_with_pickle_files_and_score_manager(self):

        srcaddr = './data/kwl_address_geo_211022.pkl'
        tgtaddr = './data/gppd_DEU_geo_211022.pkl'

        params = self.get_default_params(srcaddr, tgtaddr)
        agent = coordinator.Agent()
        agent.start(params)

    def xxx_test_coordinator_start_with_owl_and_adding_knowledge(self):

        # TODO-AE add assert
        srcaddr = './data/kwl_address_geo_211022.ttl'
        tgtaddr = './data/gppd_DEU_geo_211022.ttl'

        params = self.get_default_params(srcaddr, tgtaddr)
        agent = coordinator.Agent()
        agent.start(params)

    def test_auto_calibration_without_geo_coordinates(self):

        params_blocking = {
            "name": "TokenBasedPairIterator",
            "model_specific": {
                 "min_token_length": 3,
                 "max_token_occurrences_src": 20,
                 "max_token_occurrences_tgt": 20,
                 "blocking_properties": ["name", "isOwnedBy/hasName", "address/addressLocality", "address/streetAddress", "address/postalCode"],
                 "reset_index": False,
            }
        }

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
        sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)
        prop_prop_sim_tuples = [
            ('name', 'name', sim_fcts[4]),
            ('isOwnedBy/hasName', 'isOwnedBy/hasName', sim_fcts[0]),
            ('hasYearOfBuilt/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', sim_fcts[1]),
            ('designCapacity/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', sim_fcts[2]),
            #switched from type to fuel
            #('type', 'type', sim_fcts[3]),
            ('realizes/consumesPrimaryFuel', 'realizes/consumesPrimaryFuel', sim_fcts[3]),
        ]

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()

        matcher = coordinator.InstanceMatcherWithAutoCalibrationAgent()

        _, df_total_best_scores = matcher.start(src_onto, tgt_onto, params_blocking, prop_prop_sim_tuples=prop_prop_sim_tuples)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        #TODO-AE move this file
        matchfile = 'C:/my/tmp/ontomatch/scores_kwl_20210720_8.csv'
        # TODO-AE URGENT link type 2
        index_set_matches = evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
        logging.info('lenght of total best scores=%s, ground truth matches=%s', len(df_total_best_scores), len(index_set_matches))
        result = evaluate.evaluate(df_total_best_scores, index_set_matches)

        self.assertEqual(len(df_total_best_scores), 1298)

        expected_result = [[1.0, 1.0, 0.15948275862068967, 148, 0, 780],
            [0.9, 0.9920948616600791, 0.2704741379310345, 251, 2, 677],
            [0.8, 0.9756493506493507, 0.6476293103448276, 601, 15, 327],
            [0.7, 0.9483204134366925, 0.790948275862069, 734, 40, 194],
            [0.6, 0.8565776458951533, 0.9331896551724138, 866, 145, 62],
            [0.5, 0.7495711835334476, 0.9418103448275862, 874, 292, 54],
            [0.4, 0.6949960285941224, 0.9428879310344828, 875, 384, 53],
            [0.3, 0.6741140215716487, 0.9428879310344828, 875, 423, 53],
            [0.2, 0.6741140215716487, 0.9428879310344828, 875, 423, 53],
            [0.1, 0.6741140215716487, 0.9428879310344828, 875, 423, 53],
            [0.0, 0.6741140215716487, 0.9428879310344828, 875, 423, 53]]

        for i, expected in enumerate(expected_result):
            actual = result[i*4]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)

    def test_auto_calibration_with_geo_coordinates(self):

        params_blocking = {
            "name": "TokenBasedPairIterator",
            "model_specific": {
                 "min_token_length": 3,
                 "max_token_occurrences_src": 20,
                 "max_token_occurrences_tgt": 20,
                 "blocking_properties": ["name", "isOwnedBy/hasName", "address/addressLocality", "address/streetAddress", "address/postalCode"],
                 "reset_index": False,
            }
        }

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
        sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)
        prop_prop_sim_tuples = [
            ('name', 'name', sim_fcts[4]),
            ('isOwnedBy/hasName', 'isOwnedBy/hasName', sim_fcts[0]),
            ('hasYearOfBuilt/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', sim_fcts[1]),
            ('designCapacity/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', sim_fcts[2]),
            ('realizes/consumesPrimaryFuel', 'realizes/consumesPrimaryFuel', sim_fcts[3]),
            #TODO-AE long = x ???
            ('geo:wgs84_pos#long', 'hasGISCoordinateSystem/hasProjectedCoordinate_x/hasValue/numericalValue', sim_fcts[2]),
            ('geo:wgs84_pos#lat', 'hasGISCoordinateSystem/hasProjectedCoordinate_y/hasValue/numericalValue', sim_fcts[2])
        ]

        src_onto, tgt_onto = self.load_kwl_with_geo_coordinates_gppd_ontologies()

        matcher = coordinator.InstanceMatcherWithAutoCalibrationAgent()

        _, df_total_best_scores = matcher.start(src_onto, tgt_onto, params_blocking, prop_prop_sim_tuples=prop_prop_sim_tuples)

        logging.debug('describe dataset 1:\n%s', matcher.score_manager.get_data1().describe().to_string())
        logging.debug('describe dataset 2:\n%s', matcher.score_manager.get_data2().describe().to_string())

        #TODO-AE move this file
        matchfile = 'C:/my/tmp/ontomatch/scores_kwl_20210720_8.csv'
        # TODO-AE URGENT link type 2
        index_set_matches = evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
        logging.info('lenght of total best scores=%s, ground truth matches=%s', len(df_total_best_scores), len(index_set_matches))
        result = evaluate.evaluate(df_total_best_scores, index_set_matches)

        self.assertEqual(len(df_total_best_scores), 1298)

        expected_result = [[1.0, 1.0, 0.034482758620689655, 32, 0, 896],
            [0.9, 1.0, 0.11530172413793104, 107, 0, 821],
            [0.8, 0.977116704805492, 0.46012931034482757, 427, 10, 501],
            [0.7, 0.9730878186968839, 0.740301724137931, 687, 19, 241],
            [0.6, 0.8952282157676349, 0.9299568965517241, 863, 101, 65],
            [0.5, 0.7766051011433597, 0.9515086206896551, 883, 254, 45],
            [0.4, 0.6960629921259842, 0.9525862068965517, 884, 386, 44],
            [0.3, 0.6810477657935285, 0.9525862068965517, 884, 414, 44],
            [0.2, 0.6810477657935285, 0.9525862068965517, 884, 414, 44],
            [0.1, 0.6810477657935285, 0.9525862068965517, 884, 414, 44],
            [0.0, 0.6810477657935285, 0.9525862068965517, 884, 414, 44]]

        for i, expected in enumerate(expected_result):
            actual = result[i*4]
            self.assertAlmostEqual(actual[1], expected[1], places=2)
            self.assertAlmostEqual(actual[2], expected[2], places=2)
