import unittest
import sys
from Marie.PubChem import PubChemEngine


sys.path.append("..")


class MyTestCase(unittest.TestCase):
    def test_pubchem_engine(self):
        my_pubchem_engine = PubChemEngine()


if __name__ == '__main__':
    unittest.main()
