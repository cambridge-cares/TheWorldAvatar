import pickle

import blocking
import knowledge.geocoding
import utils_for_testing

class TestGeocoding(utils_for_testing.TestCaseOntoMatch):

    def load_kwl_gppd_ontologies(self):
        with open('./data/kwl.pkl','rb') as file:
            src_onto = pickle.load(file)
        with open('./data/gppd.pkl','rb') as file:
            tgt_onto = pickle.load(file)
        return src_onto, tgt_onto

    def test_geocoding_query(self):
        agent = knowledge.geocoding.Agent()
        lat, long = agent.query(None, 99713)
        self.assertAlmostEqual(lat, 51.259592, places=4)
        self.assertAlmostEqual(long, 10.762472, places=4)

        lat, long = agent.query('test', None)
        self.assertIsNone(lat)
        self.assertIsNone(long)

        lat, long = agent.query(None, 100000000)
        self.assertIsNone(lat)
        self.assertIsNone(long)

        lat, long = agent.query('Bad Peterstal-Griesbach', None)
        self.assertAlmostEqual(lat, 48.428963, places=4)
        self.assertAlmostEqual(long, 8.205832, places=4)

        lat, long = agent.query('Bärenbach', None)
        self.assertAlmostEqual(lat, 49.748601, places=4)
        self.assertAlmostEqual(long, 7.443275, places=4)

    def test_geocoding_kwl(self):
        kwl_onto, _ = self.load_kwl_gppd_ontologies()
        dframe = blocking.create_dataframe_from_ontology(kwl_onto)
        print(dframe.columns)
        #for i, row in dframe.iterrows():
        #    location = dframe['location']
