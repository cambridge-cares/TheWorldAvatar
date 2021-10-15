import unittest

import knowledge.geoNames
import utils_for_testing

class TestAgent(utils_for_testing.TestCaseOntoMatch):
    def test_query(self):
        agent = knowledge.geoNames.Agent()
        lat, long = agent.query(None, None)#Country feature not implemented yet
        self.assertIsNone(lat)
        self.assertIsNone(long)

        lat, long = agent.query("altbach", None)#Country feature not implemented yet
        self.assertAlmostEqual(lat, 48.76716)
        self.assertAlmostEqual(long, 12.04562)


if __name__ == '__main__':
    unittest.main()