import json
import os
import unittest
import sys
from Marie.PubChem import PubChemEngine
from Marie.Util.location import DATA_DIR

sys.path.append("..")


class MyTestCase(unittest.TestCase):
    def test_pubchem_engine(self):
        my_pubchem_engine = PubChemEngine()
        question = 'show the charge of'
        head_entity = 'CID1'
        answers = my_pubchem_engine.find_answers(question=question, head_entity=head_entity)
        best_answer = answers[0][0]
        assert (best_answer == 'CID1_charge')
        # assert (my_pubchem_engine.self_inspection() == "The full test is passed!")


    # def test_nel(self):
    #     my_pubchem_engine = PubChemEngine()
    #     question = 'what is the molar mass of C6H6'
    #     answers = my_pubchem_engine.extract_head_ent(question)
    #     print(answers)

    def test_subgraph_extraction(self):
        my_pubchem_engine = PubChemEngine()
        head_entity = 'CID23'
        subgraph = my_pubchem_engine.subgraph_extractor.retrieve_subgraph(head_entity)
        assert (subgraph == [32123, 32124, 32129, 32130, 32132, 32138, 32137, 32136,
                             32139, 32135, 32140, 32141, 32142, 32122, 32128, 32126,
                             32133, 32127, 32134, 32131, 32125])


if __name__ == '__main__':
    unittest.main()
