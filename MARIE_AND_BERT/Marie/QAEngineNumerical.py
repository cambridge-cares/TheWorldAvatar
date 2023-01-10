import json
import os
import pickle
import re
import time
import traceback
from pprint import pprint

import pandas as pd
from torch import no_grad
from transformers import BertTokenizer
import torch
from Marie.Util.NHopExtractor import HopExtractor
from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
from Marie.Util.Logging import MarieLogger
from Marie.Util.Models.TransEAScoreModel import TransEAScoreModel
from Marie.Util.location import DATA_DIR
from Marie.EntityLinking.Inference import BertNEL


class QAEngineNumerical:
    def __init__(self, dataset_dir, dataset_name, embedding="transe", dim=20, dict_type="json", largest=False):
        self.marie_logger = MarieLogger()
        self.largest = largest
        self.model_name = f"bert_{dataset_name}"
        self.dataset_dir = dataset_dir
        self.dataset_name = dataset_name
        self.subgraph_extractor = HopExtractor(dataset_dir=self.dataset_dir, dataset_name=self.dataset_name)
        self.chemical_nel = ChemicalNEL(dataset_name=self.dataset_name)
        self.device = torch.device("cpu")
        '''Load pickles for idx - label and label - idx transformation '''
        i2e_file = open(os.path.join(DATA_DIR, self.dataset_dir, 'idx2entity.pkl'), 'rb')
        self.idx2entity = pickle.load(i2e_file)
        e2i_file = open(os.path.join(DATA_DIR, self.dataset_dir, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_file)

        i2r_file = open(os.path.join(DATA_DIR, self.dataset_dir, 'idx2relation.pkl'), 'rb')
        self.idx2rel = pickle.load(i2r_file)

        if embedding == "transe":
            self.score_model = TransEAScoreModel(device=self.device,
                                                 dataset_dir=self.dataset_dir, dim=dim)
            self.score_model = self.score_model.to(self.device)
            self.score_model.load_model("bert_wikidata_numerical")

        '''Initialize tokenizer'''
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
        self.max_length = 12
        self.value_dictionary_path = os.path.join(DATA_DIR, self.dataset_dir,
                                                  f"{self.dataset_name}_value_dict.{dict_type}")
        # if dict_type == 'json':
        #     self.value_dictionary = json.loads(open(self.value_dictionary_path).read())
        # else:
        #     file = open(self.value_dictionary_path, 'rb')
        #     self.value_dictionary = pickle.load(file)

    def prepare_prediction_batch_numerical(self, question):
        """
        :param question: question in text
        :param head_entity: head entity index
        :param candidate_entities: list of candidate entity index
        :return: the rel embedding predicted, the attr embedding predicted, the numerical operator
        """
        # TODO: cache all the possible triples ...
        #
        question = question.replace("#", "")
        test_num = 10000
        test_entity = "Q170591"
        head_idx = self.entity2idx[test_entity]
        heads = torch.Tensor([head_idx])

        tails = torch.Tensor(self.subgraph_extractor.extract_neighbour_from_idx(head_idx))
        repeat_num = len(tails)
        heads = heads.repeat(repeat_num, 1).to(self.device)

        self.marie_logger.info(f" - Preparing prediction batch")
        tokenized_question_batch, tokenized_question = self.tokenize_question(question, repeat_num)
        self.marie_logger.info(f" - Question tokenized {question}")
        head_entity_batch = heads.to(self.device)
        prediction_batch = {'question': tokenized_question_batch, 'e_h': head_entity_batch,
                            'e_t': tails, 'single_question': tokenized_question}
        self.marie_logger.info(f" - Prediction batch is prepared")
        return prediction_batch

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
        return {'attention_mask': attention_mask_batch, 'input_ids': input_ids_batch}, tokenized_question

    def test_triple_distance(self):
        test_entity = "Q170591"
        rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                    header=None)
        ent_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                    header=None)

        attr_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'attr_embedding.tsv'), sep='\t',
                                     header=None)

        bias_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'bias_embedding.tsv'), sep='\t',
                                     header=None)
        print("relation label: ", self.idx2rel[60])
        rel = torch.tensor(rel_embedding.iloc[60].values).to(self.device)
        attr = torch.tensor(attr_embedding.iloc[60].values).to(self.device)
        bias = torch.tensor(bias_embedding.iloc[60].values).to(self.device)
        head_idx = self.entity2idx[test_entity]
        heads_idx = torch.Tensor([head_idx])
        tails_idx = torch.Tensor(self.subgraph_extractor.extract_neighbour_from_idx(head_idx))

        head = torch.tensor(ent_embedding.iloc[heads_idx].values).to(self.device)
        tails = torch.tensor(ent_embedding.iloc[tails_idx].values).to(self.device)

        repeat_num = len(tails)
        heads = head.repeat(repeat_num, 1).to(self.device)
        rels = rel.repeat(repeat_num, 1).to(self.device)
        # ae7b1072382624c77c75c9cb516cc29969680ad1 / 33.98
        aV = self.score_model.get_numerical_prediction(head=head, attr=attr, bias=bias)
        numerical_prediction = aV
        # numerical_prediction =  (aV + bias) * 100
        print("numerical_prediction", numerical_prediction)

        scores = self.score_model.triple_distance(heads, tails, rels)
        _, indices_top_k = torch.topk(scores, k=len(scores), largest=self.largest)
        labels_top_k = [self.idx2entity[tails_idx[index].item()] for index in indices_top_k]
        print(labels_top_k)

    def find_answers(self, question: str):
        """
        :param head_name: the standard name of the head entity
        :param k: number of results to return
        :param question: question in string format
        :param head_entity: the CID string for the head entity
        :return: score of all candidate answers
        """
        question = question.replace("'s ", " # ")

        # try:

        pred_batch = self.prepare_prediction_batch_numerical(question)
        tails = pred_batch['e_t']
        START_TIME = time.time()
        print("=============== starting the prediction ==============")
        with no_grad():
            triple_scores, numerical_predictions, numerical_operators = self.score_model.get_scores(pred_batch)
            _, indices_top_k = torch.topk(triple_scores, k=10, largest=self.largest)
            labels_top_k = [self.idx2entity[tails[index].item()] for index in indices_top_k]
            print("====== Labels =======")
            print(labels_top_k)
            print("====== Numerical Prediction =======")
            print(numerical_predictions[0] * 100)
            print("====== Numerical operator prediction =======")
            print(numerical_operators)
        print(time.time() - START_TIME)

            # self.marie_logger.info(f" prediction scores {scores}")
            # k = min(k, len(scores))
            # _, indices_top_k = torch.topk(scores, k=k, largest=self.largest)
            # labels_top_k = [self.idx2entity[candidates[index]] for index in indices_top_k]
            # scores_top_k = [scores[index].item() for index in indices_top_k]
            # targets_top_k = [head_name] * len(scores)
            # return labels_top_k, scores_top_k, targets_top_k
        # except:
        #     self.marie_logger.error('The attempt to find answer failed')
        #     self.marie_logger.error(traceback.format_exc())
        #     # return traceback.format_exc()
        #     return ["EMPTY"], [-999], ["EMPTY"]

    def extract_head_ent(self, _question):
        self.marie_logger.info("extracting head entity")
        return self.chemical_nel.find_cid(question=_question)

    def value_lookup(self, node):
        if node in self.value_dictionary:
            return self.value_dictionary[node]
        else:
            return "NODE HAS NO VALUE"

    def process_answer(self, answer_list, nel_confidence, mention_string, name, score_list):
        result_list = []
        self.marie_logger.info(f'=========== processing candidate answers ==============')
        for answer, score in zip(answer_list, score_list):
            self.marie_logger.info(f'The answer:\t\t {answer}')
            self.marie_logger.info(f'NEL confidence:\t\t  {nel_confidence}')
            self.marie_logger.info(f'Mentioned string:\t\t {mention_string}')
            self.marie_logger.info(f'Name in Dictionary:\t\t {name}')
            combined_confidence = round(min(1, nel_confidence * (1 / score)), 2)
            self.marie_logger.info(f'Confidence:\t\t {combined_confidence}')
            self.marie_logger.info(f'-------------------------')
            row = {"answer": answer, "from node": answer, "mention": mention_string,
                   "name": name, "confidence": combined_confidence}
            result_list.append(row)
        return result_list

    def run(self, question, head=None, mention=None):
        """
        :param head: directly give a head for testing and evaluation purpose.
        :param question:
        :return:
        """
        pass

        # return answer_list, score_list, target_list
        # return self.process_answer(answer_list, nel_confidence, mention_string, name, score_list)

    def remove_head_entity(self, _question, _head_entity):
        return _question.upper().replace(_head_entity.upper(), '').strip().lower()


if __name__ == "__main__":
    my_engine = QAEngineNumerical(dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical",
                                  embedding="transe",
                                  dict_type="json", dim=40)
    rst = my_engine.find_answers("vapour pressure more than")
    rst = my_engine.find_answers("vapour pressure less than")
    rst = my_engine.find_answers("vapour pressure about")
    # print(rst)
    # my_engine.test_triple_distance()
