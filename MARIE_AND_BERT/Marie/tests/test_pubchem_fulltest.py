import unittest
import sys
from Marie.PubChem import PubChemEngine

sys.path.append("..")

my_pubchem_engine = PubChemEngine()


class MyTestCase(unittest.TestCase):
    def test_pubchem_engine(self):
        question = 'show the charge of'
        head_entity = 'CID1'
        answers = my_pubchem_engine.find_answers(question=question, head_entity=head_entity)
        best_answer = answers[0][0]
        assert (best_answer == 'CID1_charge')

    def test_subgraph_extraction(self):
        head_entity = 'CID23'
        subgraph = my_pubchem_engine.subgraph_extractor.retrieve_subgraph(head_entity)
        assert (subgraph == [3171, 3172, 3177, 3178, 3180, 3186, 3185, 3184, 3187, 3183, 3188, 3189, 3190, 3170, 3176, 3174, 3181, 3175, 3182, 3179, 3173])


if __name__ == '__main__':
    unittest.main()
