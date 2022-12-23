import json

import pandas as pd
import os

# from Marie.OntoSpecies import OntoSpeciesQAEngine
# from Marie.PubChem import PubChemEngine
# from Marie.OntoCompChem import OntoCompChemEngine
# from Marie.Ontokin import OntoKinQAEngine
from Marie.CrossGraphQAEngine import CrossGraphQAEngine
from Marie.Util.location import DATA_DIR


def hit_k_rate(true_answer, pred_answers):
    scores = []
    k_list = [1, 5, 10]
    for k in k_list:
        top_k_answers = pred_answers[0: min(k, len(pred_answers)) + 1]
        if true_answer in top_k_answers:
            scores.append(1)
        else:
            scores.append(0)
    return scores


class Evaluator:

    def __init__(self):
        self.dataset_dir = os.path.join(DATA_DIR, 'CrossGraph')
        # self.pubchem_engine = PubChemEngine()
        # self.ontospecies_engine = OntoSpeciesQAEngine()
        # self.ontocompchem_engine = OntoCompChemEngine()
        # self.ontokin_engine = OntoKinQAEngine()
        self.cross_graph_test_path = os.path.join(self.dataset_dir, "cross_graph_test.tsv")
        self.cross_graph_test = pd.read_csv(self.cross_graph_test_path, sep="\t")
        # question	heads	domains	answers	mention
        self.cross_graph_test["heads"] = self.cross_graph_test["heads"].apply(eval)
        self.cross_graph_test["domains"] = self.cross_graph_test["domains"].apply(eval)
        self.cross_graph_test["answers"] = self.cross_graph_test["answers"].apply(eval)

    def calculate_accuracy(self, pred_answers, true_answers):
        """
        Calculate both hit-k rate and the recall-precision-f1 scores ...
        :param pred_answers:
        :param true_answers:
        :return:
        """
        counter = 0
        hit_1, hit_5, hit_10 = 0, 0, 0
        # Calculate the hit-k rate of the true answers
        for true_ans in true_answers:
            # remove other true answers from pred_answers
            other_true_answers = [t_a for t_a in true_ans if t_a != true_ans]
            filtered_predicted_answer = [p_a for p_a in pred_answers if p_a not in other_true_answers]
            counter += 1
            one_hit_1, one_hit_5, one_hit_10 = hit_k_rate(true_answer=true_ans, pred_answers=filtered_predicted_answer)
            print('---------')
            print("filtered_predicted_answer", filtered_predicted_answer)
            print("true answer: ", true_ans)
            print("HIT MATRIX: ", one_hit_1, one_hit_5, one_hit_10)
            hit_1 += one_hit_1
            hit_5 += one_hit_5
            hit_10 += one_hit_10

        return hit_1, hit_5, hit_10, 3

    def test_cross_graph(self):
        total_hit_1, total_hit_5, total_hit_10, total_counter = 0, 0, 0, 0
        df_test = self.cross_graph_test
        answer_dict_path = os.path.join(self.dataset_dir, "answer_dict_ablation.json")
        if os.path.exists(os.path.join(answer_dict_path)):
            answer_dict = json.loads(open(answer_dict_path).read())
            for question, answers in answer_dict.items():
                # find out the true_answers via test set
                predicted_answers = [d['node'] for d in answers if 'node' in d]
                predicted_mention = [d['target'] for d in answers if 'target' in d][0].lower()
                question_row = df_test.loc[df_test['question'] == question]
                true_domains = question_row["domains"].tolist()
                true_answers = question_row["answers"].tolist()[0]
                true_mention = question_row.iloc[0]["mention"].lower()
                print("true mention", true_mention)
                print("predicted_mention", predicted_mention)
                if "EMPTY SLOT" not in predicted_answers and predicted_mention == true_mention:
                    print(question)
                    print(predicted_answers)
                    print(true_answers)
                    hit_1, hit_5, hit_10, counter = \
                        self.calculate_accuracy(pred_answers=predicted_answers, true_answers=true_answers)
                    total_hit_1 += hit_1
                    total_hit_5 += hit_5
                    total_hit_10 += hit_10
                    total_counter += counter
                    print("-=====================")

            print("total hit 1 rate ", total_hit_1 / total_counter)
            print("total hit 5 rate", total_hit_5 / total_counter)
            print("total hit 10 rate ", total_hit_10 / total_counter)
            print("=====================")
        else:
            answer_dict = {}
            engine = CrossGraphQAEngine()
            for idx, row in self.cross_graph_test.iterrows():
                _, question, heads, domains, answers, mention = row
                heads = {}
                for h, d in zip(heads, domains):
                    heads[h] = d
                answers = engine.run(question, test=True, heads=heads)
                answer_dict[question] = answers

            with open(answer_dict_path, 'w') as f:
                f.write(json.dumps(answer_dict))

    def test_1_to_1(self, ontology, engine):
        hit_1 = 0
        hit_5 = 0
        hit_10 = 0
        ner_failure = 0
        total_counter = 0

        df_test = pd.read_csv(os.path.join(self.dataset_dir, ontology, f'{ontology}_test.tsv'), sep='\t')
        for idx, row in df_test.iterrows():
            total_counter += 1
            question, head, domain, answer, mention, rel = row
            try:
                answer_list, score_list, target_list = engine.run(question)
                target = target_list[0]
                print("target", target)
                print("mention", mention)
                try:
                    target.lower()
                    if target.lower().strip() != mention.lower().strip():
                        ner_failure += 1
                except AttributeError:
                    # The target is a nan, ner did
                    ner_failure += 1

                if answer in answer_list:
                    answer_index = answer_list.index(answer)
                    if answer_index <= 0:
                        hit_1 += 1
                    elif answer_index <= 4:
                        hit_5 += 1
                    elif answer_index <= min(len(answer_list), 10):
                        hit_10 += 1

            # TODO: check the hit rate of the answer ...
            except ValueError:
                print("failed question", question)
                ner_failure += 1

        hit_5 = hit_5 + hit_1
        hit_10 = hit_10 + hit_5
        print(f"hit 1 number {hit_1}")
        print(f"hit 5 number {hit_5}")
        print(f"hit 10 number {hit_10}")
        # =============================================
        print(f"ner failure number {ner_failure}")
        print(f"hit 1 rate {hit_1 / total_counter}")
        print(f"hit 5 rate {hit_5 / total_counter}")
        print(f"hit 10 rate {hit_10 / total_counter}")

        print(f"filtered hit 1 rate {hit_1 / (total_counter - ner_failure)}")
        print(f"filtered hit 5 rate {hit_5 / (total_counter - ner_failure)}")
        print(f"filtered hit 10 rate {hit_10 / (total_counter - ner_failure)}")


if __name__ == "__main__":
    my_evaluator = Evaluator()
    my_evaluator.test_cross_graph()
