import json
import os
import time

from Marie.OntoSpeciesNew import OntoSpeciesNew
from Marie.Util.location import DATA_DIR


class OntoSpeciesNewEvaluator:

    def __init__(self):
        ontology = "ontospecies_new"
        sub_ontology = "base_full_no_pref_selected_role_limited_100"
        self.dataset_dir = os.path.join(DATA_DIR, f"CrossGraph/{ontology}/{sub_ontology}")
        self.inference_test_set = self.load_test_files()

        self.my_engine = OntoSpeciesNew(dataset_dir=self.dataset_dir, dataset_name=sub_ontology,
                                        sub_ontology=sub_ontology, ontology=ontology)

    def inference_test(self):
        for row in self.inference_test_set:
            question, mention, true_tail, other_tails = row["question"], row["mention"], \
                                                        row["true_tail"], row["other_tails"]


            START_TIME = time.time()
            rst = self.my_engine.run(question)
            print("=================================")
            print("question:", question)
            print("result", rst)
            print("time used", time.time() - START_TIME)

    def load_test_files(self):
        # inference inference_test.json
        inference_test_set = json.loads(open(os.path.join(self.dataset_dir, "inference_test.json")).read())
        return inference_test_set

    def run(self):
        self.inference_test()


if __name__ == "__main__":
    my_evaluator = OntoSpeciesNewEvaluator()
    my_evaluator.run()
