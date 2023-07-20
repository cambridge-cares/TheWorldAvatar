import unittest
import sys
from Marie.SubgraphExtraction.SubgraphExtractor import SubgraphExtractor
sys.path.append("..")


class MyTestCase(unittest.TestCase):
    def test_initialization(self):
        my_se = SubgraphExtractor()
        self.assertIn('pubchemsupermini', my_se.load_pubchem())  # add assertion here


if __name__ == '__main__':
    unittest.main()
