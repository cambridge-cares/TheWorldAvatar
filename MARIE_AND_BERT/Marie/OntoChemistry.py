# Version 1 comes with only answer, score = (question, head)
import os
import pickle

import pandas as pd
import torch

from KGToolbox.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR
from Marie.Util.Models.OntoScoreModel import OntoScoreModel
from Marie.Util.Logging import MarieLogger
from transformers import BertTokenizer


class OntoChemistryEngine:
    def __init__(self):
        self.marie_logger = MarieLogger()
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        self.max_length = 12

        self.dataset_dir = os.path.join(DATA_DIR, 'ontocompchem_latent_40')
        self.subgraph_extractor = HopExtractor(dataset_dir=self.dataset_dir, dataset_name='ontocompchem_calculation')
        i2e_file = open(os.path.join(self.dataset_dir, 'idx2entity.pkl'), 'rb')
        self.idx2entity = pickle.load(i2e_file)
        e2i_file = open(os.path.join(self.dataset_dir, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_file)
        self.device = torch.device("cpu")
        self.ent_embedding = pd.read_csv(os.path.join(self.dataset_dir, 'ent_embedding.tsv'), sep='\t', header=None)
        self.rel_embedding = pd.read_csv(os.path.join(self.dataset_dir, 'rel_embedding.tsv'), sep='\t', header=None)
        self.score_model = OntoScoreModel(device=self.device, ent_embedding=self.ent_embedding,
                                          rel_embedding=self.rel_embedding, for_training=True,
                                          idx2entity=self.subgraph_extractor.entity_labels, load_model=False,
                                          dataset_dir=self.dataset_dir,
                                          model_name='score_model_general')

        model_path = os.path.join(self.dataset_dir, 'score_model_general')
        print("model path", model_path)
        # self.score_model.load_pretrained_model(model_path)

    def test(self):
        good_counter = 0
        bad_counter = 0
        df_test = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'ontochemistry_cross_score.tsv'), sep='\t')
        df_test = df_test.sample(frac=0.01)
        for idx, row in df_test.iterrows():
            question = row['question']
            head = row['head']
            answer = row['answer']
            labels, _ = self.run(head, question)
            print("===============")
            print()
            if answer in labels:
                good_counter += 1
            else:
                bad_counter += 1
        print('Good:', good_counter)
        print('Bad: ', bad_counter)
        # good_counter = 0
        # bad_counter = 0
        # df_test_2 = pd.read_csv(os.path.join(DATA_DIR, 'ontocompchem_latent_40', 'score_model_training.tsv'), sep='\t')
        # df_test_2 = df_test_2.sample(frac=0.01)
        # for idx, row in df_test_2.iterrows():
        #     question = row['question']
        #     head = row['head']
        #     tail = row['tail']
        #     head = self.idx2entity[head]
        #     # tail = self.idx2entity[tail]
        #     labels, _ = self.run(head, question)
        #     # labels = [l.strip() for l in labels]
        #     if tail in labels:
        #         good_counter += 1
        #     else:
        #         bad_counter += 1
        # print('Good:', good_counter)
        # print('Bad: ', bad_counter)

    def run(self, head_entity, question):
        head = self.entity2idx[head_entity]
        candidate_entities = self.subgraph_extractor.extract_neighbour_from_idx(head)
        question, head, tails = self.prepare_prediction_batch(question=question, head_entity=head,
                                                              candidate_entities=candidate_entities)
        scores = self.score_model.find_answers(question=question, head=head, tail=tails)
        k = min(5, len(tails))
        _, indices_top_k = torch.topk(scores, k=k, largest=True)
        labels_top_k = [self.idx2entity[tails[index].item()] for index in indices_top_k]
        score_top_k = [scores[index].item() for index in indices_top_k]
        return labels_top_k, score_top_k

    def tokenize_question(self, question, repeat_num):
        """
        :param question: question in text
        :param repeat_num:
        :return:
        """
        tokenized_question = self.tokenizer(question,
                                            padding='max_length', max_length=self.max_length, truncation=True,
                                            return_tensors="pt")
        attention_mask, input_ids = tokenized_question['attention_mask'], tokenized_question['input_ids']
        attention_mask_batch = attention_mask.repeat(repeat_num, 1).to(self.device)
        input_ids_batch = input_ids.repeat(repeat_num, 1).to(self.device)
        return {'attention_mask': attention_mask_batch, 'input_ids': input_ids_batch}

    def prepare_prediction_batch(self, question, head_entity, candidate_entities):
        """
        :param question: question in text
        :param head_entity: head entity index
        :param candidate_entities: list of candidate entity index
        :return: Ranked list of candidate entities
        """
        self.marie_logger.info(f" - Preparing prediction batch")
        candidate_entities = torch.LongTensor(candidate_entities).to(self.device)
        self.marie_logger.info(f" - Candidate entities: {candidate_entities}")
        repeat_num = len(candidate_entities)
        tokenized_question_batch = self.tokenize_question(question, repeat_num)
        self.marie_logger.info(f" - Question tokenized {question}")
        head_entity_batch = torch.LongTensor([head_entity]).repeat(repeat_num).to(self.device)
        self.marie_logger.info(f" - Head entity index {head_entity}")
        prediction_batch = (tokenized_question_batch, head_entity_batch, candidate_entities)
        self.marie_logger.info(f" - Prediction batch is prepared")
        return prediction_batch


# 1. Subgraph extractor
# 2. Score model
# 3.
if __name__ == '__main__':
    my_ontochemistry_engine = OntoChemistryEngine()
    my_ontochemistry_engine.test()
