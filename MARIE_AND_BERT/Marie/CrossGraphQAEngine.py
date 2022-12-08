import os
import torch
from torch.nn.functional import one_hot
from transformers import BertTokenizer
from Marie.PubChem import PubChemEngine
from Marie.OntoCompChem import OntoCompChemEngine
from Marie.OntoSpecies import OntoSpeciesQAEngine
from Marie.Ontokin import OntoKinQAEngine
from Marie.Util.Logging import MarieLogger
from Marie.Util.Models.CrossGraphAlignmentModel import CrossGraphAlignmentModel
from Marie.Util.location import DATA_DIR


def normalize_scores(scores_list):
    print(scores_list)
    normalized_scores = []
    for scores in scores_list:
        max_score = max(scores)
        scores = [score / max_score for score in scores]
        if len(scores) < 5:
            diff = 5 - len(scores)
            for i in range(diff):
                scores.append(-999)
        normalized_scores.append(scores)
    return normalized_scores


class CrossGraphQAEngine:
    """
    The cross graph QA engine will answer question across domains
    """

    def __init__(self):
        self.marie_logger = MarieLogger()
        self.pubchem_engine = PubChemEngine()
        self.ontochemistry_engine = OntoCompChemEngine()
        self.ontospecies_engine = OntoSpeciesQAEngine()
        self.ontokin_engine = OntoKinQAEngine()

        self.domain_encoding = {"pubchem": 0, "ontocompchem": 1, "ontospecies": 2, "ontokin": 3}
        self.encoding_domain = {v: k for k, v in self.domain_encoding.items()}
        self.domain_list = self.domain_encoding.keys()
        print(self.domain_list)
        self.engine_list = [self.pubchem_engine, self.ontochemistry_engine, self.ontospecies_engine,
                            self.ontokin_engine]
        self.device = torch.device("cpu")
        self.max_length = 12
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        self.dataset_path = os.path.join(DATA_DIR, 'CrossGraph')
        self.score_adjust_model = CrossGraphAlignmentModel(device=self.device).to(self.device)
        self.score_adjust_model.load_state_dict(torch.load(os.path.join(self.dataset_path,
                                                                        'cross_graph_model_new'),
                                                           map_location=self.device))


    def create_triple_for_prediction(self, question, score_list, domain_list):
        # try:
        score_list = torch.FloatTensor(score_list).reshape(1, -1).squeeze(0)
        domain_list = torch.LongTensor(domain_list)
        tokenized_question = self.tokenizer(question,
                                            padding='max_length', max_length=self.max_length, truncation=True,
                                            return_tensors="pt")
        question_list = {}
        for key in tokenized_question:
            data = tokenized_question[key].repeat([1, len(score_list)])
            question_list[key] = data

        return question_list, score_list, domain_list

    def adjust_scores(self, triple):
        return self.score_adjust_model.predict(triple=triple)

    def re_rank_answers(self, domain_list, score_list, answer_list, target_list):
        values, indices = torch.topk(torch.FloatTensor(score_list), k=10, largest=True)
        re_ranked_labels = [answer_list[i] for i in indices]
        re_ranked_domain_list = [domain_list[i] for i in indices]
        re_ranked_engine_list = [self.engine_list[domain] for domain in re_ranked_domain_list]
        re_ranked_score_list = [float(v.item() / 1.5) for v in values]
        re_ranked_domain_list = [self.encoding_domain[d] for d in re_ranked_domain_list]
        re_ranked_node_value_list = [engine.value_lookup(node) for engine, node in
                                     zip(re_ranked_engine_list, re_ranked_labels)]

        result = []

        for label, domain, score, value, target in zip(re_ranked_labels, re_ranked_domain_list, re_ranked_score_list,
                                                       re_ranked_node_value_list, target_list):
            row = {"node": label, "domain": domain, "score": score, "value": value, "target": target}
            result.append(row)

        return result

    def run(self, question):
        """
        The main interface for the integrated QA engine
        :param head_entity_list:
        :param question: question in string, with head entity removed
        :param head_entity: IRI of the head entity before cross-ontology translation, always in the form of CID (pubchem ID)
        :return: the re-ranked list of answer labels according to the adjusted scores
        """
        # question = question.replace(" of ", " ")
        score_list = []
        label_list = []
        domain_list = []
        target_list = []
        print("=========================")
        for domain, engine in zip(self.domain_list, self.engine_list):
            labels, scores, targets = engine.run(question=question)
            length_diff = 5 - len(labels)
            scores = scores + [-999] * length_diff
            labels = labels + ["EMPTY SLOT"] * length_diff
            targets = targets + ["EMPTY SLOT"] * length_diff
            score_list.append(scores)
            label_list += labels
            target_list += targets
            for i in range(max(len(labels), 5)):
                domain_list.append(int(self.domain_encoding[domain]))
        print("score length", len(score_list))
        print("label length", len(label_list))
        print("domain length", len(domain_list))
        score_list = normalize_scores(score_list)
        encoded_domain_list = torch.LongTensor(domain_list)
        encoded_domain_list = one_hot(encoded_domain_list, num_classes=4)

        triples = self.create_triple_for_prediction(question=question, score_list=score_list,
                                                    domain_list=encoded_domain_list)
        print("triples length", len(triples))

        score_factors = self.adjust_scores(triples)

        adjusted_score_list = []
        for score, score_factor, domain in zip(score_list, score_factors, self.domain_list):
            score = torch.FloatTensor(score)
            adjusted_score = score + score_factor
            adjusted_score = adjusted_score.tolist()
            adjusted_score_list = adjusted_score_list + adjusted_score
            print("------------------------")
            print("domain:", domain)

        return self.re_rank_answers(domain_list=domain_list, score_list=adjusted_score_list, answer_list=label_list,
                                    target_list=target_list)


if __name__ == '__main__':
    my_qa_engine = CrossGraphQAEngine()
    my_qa_engine.run(question="what is the geometry of C6H6")
    # x = ""
    # while x != "exit":
    #     x = input("question:")
    #     START_TIME = time.time()
    #     my_qa_engine.run(question=x)
    #     print(time.time() - START_TIME)
