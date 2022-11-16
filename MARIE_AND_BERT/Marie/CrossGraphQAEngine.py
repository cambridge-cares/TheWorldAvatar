import os
import pandas as pd
import torch
from transformers import BertTokenizer

from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
from Marie.PubChem import PubChemEngine
from Marie.OntoChemistry import OntoChemistryEngine
from Marie.Util.Logging import MarieLogger
from Marie.Util.Models.CrossGraphAlignmentModel import CrossGraphAlignmentModel
from Marie.Util.location import DATA_DIR


def normalize_scores(scores_list):
    normalized_scores = []
    for scores in scores_list:
        max_score = max(scores)
        scores = [score / max_score for score in scores]
        normalized_scores.append(scores)
    return normalized_scores


class CrossGraphQAEngine:
    """
    The cross graph QA engine will answer question across domains
    """

    def __init__(self):
        self.marie_logger = MarieLogger()
        self.chemical_nel = ChemicalNEL()

        self.pubchem_engine = PubChemEngine()
        self.ontochemistry_engine = OntoChemistryEngine()
        self.domain_list = ["pubchem", "ontochemistry"]
        self.entity_dict = {"ontochemistry": {
            "CID22": "bf11a071-8f0e-3f88-9e7e-986b9983d278"
        }}
        self.device = torch.device("cpu")
        self.max_length = 12
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        self.dataset_path = os.path.join(DATA_DIR, 'CrossGraph')
        self.score_adjust_model = CrossGraphAlignmentModel(device=self.device)
        self.score_adjust_model.load_state_dict(torch.load(os.path.join(self.dataset_path,
                                                                        'cross_graph_model')))

    def create_triple_for_prediction(self, question, score_list, domain_list):
        score_list = torch.FloatTensor(score_list).reshape(1, -1).squeeze(0)
        domain_list = torch.LongTensor(domain_list)
        tokenized_question = self.tokenizer(question,
                                            padding='max_length', max_length=self.max_length, truncation=True,
                                            return_tensors="pt")
        question_list = {}
        for key in tokenized_question:
            data = tokenized_question[key].repeat([1, 10])
            question_list[key] = data

        return question_list, score_list, domain_list


    def extract_head_ent(self, _question):
        self.marie_logger.info("extracting head entity")
        return self.chemical_nel.find_cid(question=_question)

    def adjust_scores(self, triple):
        return self.score_adjust_model.predict(triple=triple)

    def run(self, question, head_entity):
        """
        The main interface for the integrated QA engine
        :param question: question in string, with head entity removed
        :param head_entity: IRI of the head entity before cross-ontology translation, always in the form of CID (pubchem ID)
        :return: the re-ranked list of answer labels according to the adjusted scores
        """
        score_list = []
        label_list = []
        domain_list = []
        for domain in self.domain_list:
            head_entity = self.link_entity(entity=head_entity, domain=domain)
            if domain == "pubchem":
                labels, scores = self.pubchem_engine.find_answers(question=question, head_entity=head_entity)
                for s in scores:
                    domain_list.append(0)
                scores = [10 - score for score in scores]
            elif domain == "ontochemistry":
                labels, scores = self.ontochemistry_engine.run(question=question, head_entity=head_entity)
                for s in scores:
                    domain_list.append(1)
            else:
                labels, scores = [], []
            score_list.append(scores)
            label_list.append(labels)

        score_list = normalize_scores(score_list)
        triples = self.create_triple_for_prediction(question=question, score_list=score_list, domain_list=domain_list)
        adjusted_scores = self.adjust_scores(triples)
        print(adjusted_scores)

    def link_entity(self, entity, domain):
        """
        Link to the IRI of the entity (using CID as the universal identifier)
        :param entity: the entity IRI in the form of CID
        :param domain: the label of the domain in string e.g. pubchem, ontochemistry
        :return: the entity IRI in the given domain e.g. bf11a071-8f0e-3f88-9e7e-986b9983d278, CID22
        for pubchem, the entity remains unchanged, for ontochemistry, it needs to be converted to ontospecies IRI
        """
        if domain == "pubchem":
            return entity
        elif domain == "ontochemistry":
            return entity
            # return self.entity_dict[domain][entity]
        else:
            return entity

    def test(self):
        """
        Use cross_graph_pairs.tsv to test the integrated system
        :return:
        """
        # question	head	domain	answer
        test_set = pd.read_csv(os.path.join(self.dataset_path, 'all_cross_score.tsv'), sep='\t')
        for idx, row in test_set.iterrows():
            question, head, domain, answer = row
            self.run()

if __name__ == '__main__':
    my_qa_engine = CrossGraphQAEngine()
    my_qa_engine.test()
