import unittest
import sys

sys.path.append("..")

from Marie.EntityLinking.ChemicalNEL import ChemicalNEL


class MyTestCase(unittest.TestCase):
    def test_nel(self):
        cnl = ChemicalNEL()
        rst_1 = cnl.find_cid('what is the molar mass of water')
        print(rst_1)
        rst_2 = cnl.find_cid('blah blah blah something')
        print(rst_2)
        rst_3 = cnl.find_cid('blah blah blah something C6 H12')
        print(rst_3)
        rst_4 = cnl.find_cid('blah blah blah something  H12C6')
        print(rst_4)
        rst_5 = cnl.find_cid('blah blah blah something H4C2O1')
        print(rst_5)


if __name__ == '__main__':
    unittest.main()
