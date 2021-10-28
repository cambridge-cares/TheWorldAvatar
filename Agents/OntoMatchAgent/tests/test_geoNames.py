import unittest

import knowledge.geoNames

class TestAgent(unittest.TestCase):

    def test_query_ep(self):
        agent = knowledge.geoNames.Agent("Germany")

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
if __name__ == '__main__':
    unittest.main()