from ssl import get_default_verify_paths
from alignment import Alignment
import coordinator
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

        #directory = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/'
        #directory = 'C:/my/tmp/ontomatch/tmp_kwl_files/'
        #srcaddr = directory + 'kwl.owl'
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

    def test_coordinator_load_with_pickle_files(self):

        # TODO-AE add assert commands or extend test

        srcaddr = './data/kwl_geo.pkl'
        tgtaddr = './data/gppd.pkl'

        agent = coordinator.Agent()
        agent.load(srcaddr, tgtaddr)

    def test_coordinator_start_with_pickle_files_and_score_manager(self):

        srcaddr = './data/kwl_geo.pkl'
        tgtaddr = './data/gppd.pkl'

        params = self.get_default_params(srcaddr, tgtaddr)
        agent = coordinator.Agent()
        agent.start(params)

    def xxxtest_coordinator_start_with_owl_and_adding_knowledge(self):

        # TODO-AE remove this test or adapt directories

        directory = 'C:/my/tmp/ontomatch/tmp_kwl_files/'
        srcaddr = directory + 'kwl_address.owl'
        directory = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/'
        tgtaddr = directory + 'gppd0722.owl'

        params = self.get_default_params(srcaddr, tgtaddr)
        agent = coordinator.Agent()
        agent.start(params)
