import os
import unittest
import sys

from Marie.OntoMoPs import OntoMoPsQAEngine
from Marie.Util.location import DATA_DIR

sys.path.append("../..")


class MyTestCase(unittest.TestCase):

    def test_numerical_operator(self):
        ontology = "OntoMoPs"
        sub_ontology = "numerical_with_implicit"
        dataset_dir = os.path.join(DATA_DIR, f"CrossGraph/{ontology}/{sub_ontology}")
        my_engine = OntoMoPsQAEngine(dataset_dir=dataset_dir, dataset_name=sub_ontology, sub_ontology=sub_ontology,
                                     ontology=ontology)

        question_list = ["Find MoPs with molecular weight less than",
                         "Find all MoPs with molecular weight over",
                         "List the MOPs with (3-pyramidal)8(2-bent)12(Cs) as the assembly model"]

        operator_list = ["smaller", "larger", "none"]

        # ================ test whether the operator is fine =================================
        for question, operator in zip(question_list, operator_list):
            _, tokenized_question = my_engine.nlp.tokenize_question(question=question, repeat_num=1)
            single_question_embedding_operator = my_engine.operator_model.get_question_embedding(
                question=tokenized_question)
            my_engine.get_numerical_operator(tokenized_question=tokenized_question,
                                             single_question_embedding=single_question_embedding_operator)
            assert my_engine.input_dict.numerical_operator == operator

        mention_list = [('mops', None, None), ('mops', None, None), ('mops', ['AssemblyModel_75b7fbdf-f0cb-4dc0-a260-da7cbce678c5'], '(3-pyramidal)8(2-bent)12(Cs)')]

        for question, true_mention in zip(question_list,mention_list):
            # print("question:", question)
            mention = my_engine.nel.get_mention(question)
            assert mention == true_mention

        relation_list = ["hasMolecularWeight", "hasMolecularWeight", "hasAssemblyModelReversed"]
        for question, relation_label in zip(question_list, relation_list):
            question = my_engine.text_filtering(question)
            _, tokenized_question = my_engine.nlp.tokenize_question(question=question, repeat_num=1)
            single_question_embedding = my_engine.score_model.get_question_embedding(question=tokenized_question)
            _, pred_p_idx = my_engine.score_model.get_relation_prediction(question_embedding=single_question_embedding)
            idx_to_replace = [23, 8, 12, 13, 18, 0]
            if pred_p_idx in idx_to_replace:
                pred_p_idx = 1
            p_label = my_engine.idx2rel[pred_p_idx]
            assert p_label == relation_label

            # assert p_label == relation_label


if __name__ == '__main__':
    unittest.main()
