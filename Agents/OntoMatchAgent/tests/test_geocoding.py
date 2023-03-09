import logging

import ontomatch.blocking
import ontomatch.knowledge.geocoding
import tests.utils_for_testing

class TestGeocoding(tests.utils_for_testing.TestCaseOntoMatch):

    def test_geocoding_query(self):
        agent = ontomatch.knowledge.geocoding.Agent()
        lat, long = agent.query(None, 91602)
        self.assertAlmostEqual(lat, 49.108758, places=4)
        self.assertAlmostEqual(long, 10.386423, places=4)

        lat, long = agent.query('test', None)
        self.assertIsNone(lat)
        self.assertIsNone(long)

        lat, long = agent.query(None, 100000000)
        self.assertIsNone(lat)
        self.assertIsNone(long)

        lat, long = agent.query('Bad Peterstal-Griesbach', None)
        self.assertAlmostEqual(lat, 48.428963, places=4)
        self.assertAlmostEqual(long, 8.205832, places=4)

        lat, long = agent.query('Bälau', None)
        self.assertAlmostEqual(lat, 53.616381, places=4)
        self.assertAlmostEqual(long, 10.621739, places=4)

    def test_geocoding_kwl(self):
        ontosrc, ontotgt = self.load_kwl_gppd_ontologies()
        dfsrc = ontomatch.blocking.create_dataframe_from_ontology(ontosrc)
        columns =  [ str(c) for c in dfsrc.columns ]
        logging.info('columns=%s', columns)
        self.assertIn('address/postalCode', columns)
