import json
import os
import unittest
import sys

from Marie.CandidateSelection.location import DATA_DIR
from Marie.QAEngineNumerical import QAEngineNumerical

sys.path.append("..")

engine = QAEngineNumerical(dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical",
                           embedding="transe",
                           dict_type="json", dim=40, test=False)


def test_single(question, mention=None):
    rst = engine.run(question, mention=mention)
    print(rst)


class MyTestCase(unittest.TestCase):

    def test_something(self):
        self.assertEqual(True, True)  # add assertion here

    def test_normal(self, limit_num=-1):
        start_idx = 0
        result_list = []
        test_set_path = os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical",
                                     "wikidata_numerical_test_set-normal_smaller.json")
        counter = start_idx
        with open(test_set_path) as f:
            test_set = json.loads(f.read())
            for question, s, p, o, _, species_label, _, _ in test_set[start_idx:limit_num]:
                # for question, s, p, o, _, species_label, _, _ in test_set:
                counter += 1
                print("=======================================================")
                print(f"Evaluating question {question} - Number {counter} out of len {len(test_set)}")
                rst = engine.find_answers(question, true_p=p, true_operator=None, true_head=s,
                                          species_label=species_label)
                result_list.append(rst)

        result_path = os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical",
                                   "wikidata_numerical_test_result_normal_smaller.json")

        with open(result_path, "w") as f:
            f.write(json.dumps(result_list))
            f.close()

    def test_numerical(self):
        result_list = []
        test_set_path = os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical",
                                     "wikidata_numerical_test_set-numerical.json")
        with open(test_set_path) as f:
            test_set = json.loads(f.read())
            # for question, _, true_p, _, _, _, true_operator in random.sample(test_set, 100):
            for question, _, true_p, _, _, _, true_operator in test_set:
                print(f"Evaluating question {question}")
                rst = engine.find_answers(question, true_p=true_p, true_operator=true_operator)
                result_list.append(rst)
        result_path = os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical", "wikidata_numerical_test_result_3.json")

        with open(result_path, "w") as f:
            f.write(json.dumps(result_list))
            f.close()


if __name__ == '__main__':
    unittest.main()
