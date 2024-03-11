import os
import unittest
import sys

from Marie.CandidateSelection.location import DATA_DIR

sys.path.append("..")

from Marie.EntityLinking.ChemicalNEL import ChemicalNEL


class MyTestCase(unittest.TestCase):

    def test_normal_nel(self):
        nel = ChemicalNEL()
        rst = nel.get_mention("What is the molecular weight of CH5")
        assert rst == "CH5"
        rst = nel.get_mention("Find the charge of benzene and CH5")
        assert rst == "CH5"
        rst = nel.get_mention("Find the conductive of benzene")
        assert rst == "benzene"
        rst = nel.get_mention("Find species with molecular weight more than 100 g/mol")
        assert rst == "g/mol"


    def test_multi_target_nel(self):
        dataset_name = "OntoMoPs"

        multi_target_nel = ChemicalNEL(dataset_name=dataset_name, enable_class_ner=True)
        rst = multi_target_nel.get_mention("MoPs with molecular weight less than 100")
        assert rst == ('mops', None, None)
        rst = multi_target_nel.get_mention("List the MOPs with (3-pyramidal)8(2-bent)12(Cs) as the assembly model")
        assert rst == ('mops', ['AssemblyModel_75b7fbdf-f0cb-4dc0-a260-da7cbce678c5'], '(3-pyramidal)8(2-bent)12(Cs)')


if __name__ == '__main__':
    unittest.main()
