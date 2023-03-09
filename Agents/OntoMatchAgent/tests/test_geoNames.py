import unittest

import ontomatch.knowledge.geoNames

class TestAgent(unittest.TestCase):

    def test_pkl(self):
        agent = ontomatch.knowledge.geoNames.Agent("UnitedKingdom", isOnline=False, save = "./tests/data/UK_geonames.pkl")
        lat, long = agent.query("Norwich")
        self.assertAlmostEqual(lat, 52.62783)
        self.assertAlmostEqual(long, 1.29834)

    def test_query_offline(self):
        agent = ontomatch.knowledge.geoNames.Agent("Germany")

        #test base case
        lat, long = agent.query("altbach")
        self.assertAlmostEqual(lat, 48.7225)
        self.assertAlmostEqual(long, 9.379)

        #test empty
        lat, long = agent.query("")
        self.assertIsNone(lat)
        self.assertIsNone(long)

        #test alternative name
        lat, long = agent.query("Gera-Zwötzen")
        self.assertAlmostEqual(lat, 50.84858)
        self.assertAlmostEqual(long, 12.08635)

    def test_query_online(self):
        agent = ontomatch.knowledge.geoNames.Agent("Germany", isOnline=True)

        # test base case
        lat, long = agent.query("altbach")
        self.assertAlmostEqual(lat, 48.7225)
        self.assertAlmostEqual(long, 9.379)

        # test empty
        lat, long = agent.query("")
        self.assertIsNone(lat)
        self.assertIsNone(long)

        # test alternative name
        lat, long = agent.query("Gera-Zwötzen")
        self.assertAlmostEqual(lat, 50.84858)
        self.assertAlmostEqual(long, 12.08635)


    def test_query_UK(self):
        agent = ontomatch.knowledge.geoNames.Agent("UnitedKingdom", isOnline=True)

        # test base case
        lat, long = agent.query("Norwich")
        self.assertAlmostEqual(lat, 52.62783)
        self.assertAlmostEqual(long, 1.29834)

if __name__ == '__main__':
    unittest.main()