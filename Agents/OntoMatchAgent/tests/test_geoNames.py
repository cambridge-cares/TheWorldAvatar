import unittest
import knowledge.geoNames

class TestAgent(unittest.TestCase):
    def test_query(self):
        agent = knowledge.geoNames.Agent()
        lat, long = agent.query(None, None)
        self.assertIsNone(lat)
        self.assertIsNone(long)


        lat, long = agent.query("altbach", "germany")
        self.assertAlmostEqual(lat, 48.76716)
        self.assertAlmostEqual(long, 12.04562)

        lat, long = agent.query("altbach", "germany")
        self.assertAlmostEqual(lat, 48.76716)
        self.assertAlmostEqual(long, 12.04562)

    def test_query_remote(self):
        agent = knowledge.geoNames.Agent()
        lat, long = agent.query("Aarbergen", "germany")
        self.assertAlmostEqual(lat, 50.25118)
        self.assertAlmostEqual(long, 8.08905)

    def test_save(self):
        agent = knowledge.geoNames.Agent()
        lat, long = agent.query("moers", "germany")
        agent.onclose()
        self.assertTrue(lat)


if __name__ == '__main__':
    unittest.main()