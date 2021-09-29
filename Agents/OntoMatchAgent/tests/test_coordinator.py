import logging

import coordinator
import util
import utils_for_testing

class TestCoordinator(utils_for_testing.TestCaseOntoMatch):

    def test_coordinator_load_add_knowledge(self):

        #directory = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/'
        directory = 'C:/my/tmp/ontomatch/tmp_kwl_files/'
        srcaddr = directory + 'kwl.owl'

        agent = coordinator.Agent()
        graph = agent.load_rdflib_graph(srcaddr, add_knowledge=True)

        query = '''
        PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
        SELECT ?subj ?lat ?long
        WHERE {
            ?subj geo:lat ?lat .
            ?subj geo:long ?long .
        }'''

        result = list(graph.query(query))
        self.assertGreater(len(result), 1500)

        onto = agent.load_owlready2_ontology(graph)
        self.assertIsNotNone(onto)

    def xxxtest_coordinator_load(self):

        directory = 'C:/my/tmp/ontomatch/tmp_kwl_files/'
        srcaddr = directory + 'kwl.owl'
        directory = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/'
        tgtaddr = directory + 'gppd0722.owl'

        agent = coordinator.Agent()
        agent.load(srcaddr, tgtaddr, add_knowledge=True, dump_ontology=True)

    def test_coordinator_load_with_pickle_files(self):

        directory = 'C:/my/tmp/ontomatch/tmp_kwl_files/'
        srcaddr = directory + 'kwl_with_geo.pkl'
        directory = 'C:/my/tmp/ontomatch/20210923_testdata_from_shaocong/kwlVSgppd/'
        tgtaddr = directory + 'gppd0722.pkl'

        agent = coordinator.Agent()
        agent.load(srcaddr, tgtaddr)
