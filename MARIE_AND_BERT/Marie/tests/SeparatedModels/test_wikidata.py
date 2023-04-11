import unittest
import sys

from Marie.WikidataEngine import WikidataEngine

sys.path.append("../..")


class MyTestCase(unittest.TestCase):
    def test_numerical_operator(self):
        my_engine = WikidataEngine(dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical")
        question_list = ["what is the smiles string of CH4O4S",
                         "find species with boiling point larger than 10 degrees",
                         "find species with boiling point smaller than 10 degrees",
                         "find species with boiling point around 100 degrees"]

        operator_list = ["none", "larger", "smaller", "about"]

        for question, operator in zip(question_list, operator_list):
            _, tokenized_question = my_engine.nlp.tokenize_question(question=question, repeat_num=1)
            single_question_embedding_operator = my_engine.score_model.get_question_embedding(
                question=tokenized_question)
            my_engine.get_numerical_operator(tokenized_question=tokenized_question,
                                             single_question_embedding=single_question_embedding_operator)

            print("predicted operator", my_engine.input_dict.numerical_operator)
            print("operator:", operator)
            assert my_engine.input_dict.numerical_operator == operator

        for question in question_list:
            mention = my_engine.nel.get_mention(question)
            print("===========================")
            print("mention:", mention)
            print("question:", question)
            print("===========================")

            # assert mention == true_mention


if __name__ == '__main__':
    unittest.main()
