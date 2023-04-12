import os
import unittest
import sys

from Marie.EntityLinking.ChemicalNEL import ChemicalNEL

sys.path.append("..")


class MyTestCase(unittest.TestCase):

    def test_get_mention_pce_agent(self):
        nel = ChemicalNEL(dataset_name=os.path.join("ontoagent", "pceagent"), enable_class_ner=False)
        question_list = ["what is the power conversion efficiency of NSC2559",
                         "what is the pce of benzene",
                         "what is the pce of CS-W004520",
                         "what is the gibbs energy of CCCCL"]


        mention_list = [
            "NSC2559",
            "benzene",
            "CS-W004520",
            "CCCCL",
        ]
        iri_list = [
            (0.7142857142857143,
             'http://www.theworldavatar.com/kg/ontospecies/Species_b54e77b6-eb98-4a0e-b73e-b9761977bdc4', 'NSC2559',
             'C5H9NS'),
            (1, 'http://www.theworldavatar.com/kg/ontospecies/Species_a04c658b-5377-4c41-a59c-a810e1885c28', 'benzene', 'BENZENE'),
            (1, 'http://www.theworldavatar.com/kg/ontospecies/Species_8038971a-adee-47d3-a8de-ef976c4de264', 'CS-W004520', 'CS-W004520'),
            (1, 'http://www.theworldavatar.com/kg/ontospecies/Species_092899be-282a-4470-a0e2-c9e7f1d81f90', 'CCCCL', 'CCCCL')
        ]

        for question, true_iri, true_mention in zip(question_list, iri_list, mention_list):
            mention = nel.get_mention(question)
            iri = nel.find_cid(mention)
            assert mention == true_mention
            # print("============================================")
            # print("Question:", question)
            # print("Mention:", mention)
            assert iri == true_iri
            # print("IRI:", iri)
            # print("============================================")


    def test_get_mention_thermal_agent(self):
        nel = ChemicalNEL(dataset_name=os.path.join("ontoagent", "thermoagent"), enable_class_ner=False)

        question_list = ["what is the heat capacity of benzene at temperature of 100K",
                         "find the thermal properties of C=C=C=C",
                         "what is the heat capacity of benzene",
                         "show me the internal energy of CH4",
                         "what is the gibbs energy of BENZENE"]

        mention_list = [
            "C=C=C=C",
            "benzene",
            "CH4",
            "BENZENE",
                        ]
        iri_list = [
            (1, 'http://www.theworldavatar.com/kb/ontospecies/Species_c7bc0de0-6b09-45cf-9bb4-3c8eadfd9f47', 'C=C=C=C', 'C=C=C=C'),
            (1, 'http://www.theworldavatar.com/kb/ontospecies/Species_a04c658b-5377-4c41-a59c-a810e1885c28', 'benzene', 'BENZENE'),
            (1, 'http://www.theworldavatar.com/kb/ontospecies/Species_1e7d8a91-2ea9-4c75-b9ed-8396f13573bb', 'CH4','CH4'),
            (1, 'http://www.theworldavatar.com/kb/ontospecies/Species_a04c658b-5377-4c41-a59c-a810e1885c28', 'BENZENE', 'BENZENE')
        ]

        for question, true_iri, true_mention in zip(question_list, iri_list, mention_list):
            mention = nel.get_mention(question)
            iri = nel.find_cid(mention)
            # assert mention == true_mention
            print("============================================")
            print("Question:", question)
            # assert iri == true_iri
            print("IRI:", iri)
            print("============================================")


if __name__ == '__main__':
    unittest.main()
