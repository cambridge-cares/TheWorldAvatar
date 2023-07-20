import os
import unittest
import sys

from Marie.CandidateSelection.location import DATA_DIR
from Marie.OntoSpeciesNew import OntoSpeciesNew
from Marie.QAEngineNumerical import QAEngineNumerical

sys.path.append("../..")


class MyTestCase(unittest.TestCase):

    def test_numerical_prediction(self):
        operator_dict = {0: "smaller", 1: "larger", 2: "none"}
        ontology = "ontospecies_new"
        sub_ontology = "base_full_no_pref_selected_role_limited_100"
        dataset_dir = os.path.join(DATA_DIR, f"CrossGraph/{ontology}/{sub_ontology}")
        my_engine = OntoSpeciesNew(dataset_dir=dataset_dir, dataset_name=sub_ontology, sub_ontology=sub_ontology,
                                   ontology=ontology, operator_dict=operator_dict)

        question_list = ["species with molecular weight less than 100",
                         "species with molecular weight over 100",
                         "boiling point of benzene",
                         "what is the boiling point of benzene",
                         "find species with molecular weight less than 100"]

        operator_list = ["smaller", "larger", "none", "none", "smaller"]

        for question, operator in zip(question_list, operator_list):
            _, tokenized_question = my_engine.nlp.tokenize_question(question=question, repeat_num=1)
            single_question_embedding_operator = my_engine.operator_model.get_question_embedding(
                question=tokenized_question)
            my_engine.get_numerical_operator(tokenized_question=tokenized_question,
                                             single_question_embedding=single_question_embedding_operator)
            assert my_engine.input_dict.numerical_operator == operator

        mention_list = [('species', None, None), ('species', None, None),
                        (None, ['Species_a04c658b-5377-4c41-a59c-a810e1885c28'], 'benzene'),
                        (None, ['Species_a04c658b-5377-4c41-a59c-a810e1885c28'], 'benzene'),
                        ('species', None, None)]

        for question, true_mention in zip(question_list, mention_list):
            mention = my_engine.nel.get_mention(question)
            assert mention == true_mention

        relation_label_list = ["hasMolecularWeight", "hasMolecularWeight", "hasBoilingPoint", "hasBoilingPoint",
                               "hasMolecularWeight"
                               ]
        for question, relation_label in zip(question_list, relation_label_list):
            question = my_engine.text_filtering(question)
            _, tokenized_question = my_engine.nlp.tokenize_question(question=question, repeat_num=1)
            single_question_embedding = my_engine.score_model.get_question_embedding(question=tokenized_question)
            _, pred_p_idx = my_engine.score_model.get_relation_prediction(question_embedding=single_question_embedding)
            p_label = my_engine.idx2rel[pred_p_idx]
            assert p_label == relation_label



if __name__ == '__main__':
    unittest.main()
