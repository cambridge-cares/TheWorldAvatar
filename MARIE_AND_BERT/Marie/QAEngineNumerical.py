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

    @staticmethod
    def numerical_value_extractor(question):
        numerical_values = re.findall(r"[0-9.]+", question)
        if len(numerical_values) > 0:
            return float(numerical_values[0]), numerical_values[0]
        else:
            return None, None

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

        all_species_indices_file = open(os.path.join(DATA_DIR, self.dataset_dir, 'all_species.pkl'), 'rb')
        self.all_species_indices = pickle.load(all_species_indices_file)

        self.operator_dict = {0: "smaller", 1: "larger", 2: "near", 3: "none"}

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
        if dict_type == 'json':
            self.value_dictionary = json.loads(open(self.value_dictionary_path).read())
        else:
            file = open(self.value_dictionary_path, 'rb')
            self.value_dictionary = pickle.load(file)

    def value_lookup(self, node):
        if node in self.value_dictionary:
            return self.value_dictionary[node]
        else:
            return "NODE HAS NO VALUE"

    def prepare_prediction_batch_numerical(self, heads, question):
        """
        :param heads:
        :param question: question in text
        :return: the rel embedding predicted, the attr embedding predicted, the numerical operator
        """
        # TODO: cache all the possible triples ...

        heads_batch = []
        tails_batch = []
        split_indices = []

        for head in heads:
            # find the neighbours of this head
            # append the neighbours to tail_batch
            neighbours = self.subgraph_extractor.extract_neighbour_from_idx(head)
            heads_batch += [head] * len(neighbours)
            tails_batch += neighbours
            split_indices.append(len(tails_batch))

        self.marie_logger.info(f" - Preparing prediction batch")
        prediction_batch = {'e_h': torch.LongTensor(heads_batch),
                            'e_t': torch.LongTensor(tails_batch), 'single_question': question}
        self.marie_logger.info(f" - Prediction batch is prepared")
        return prediction_batch, torch.LongTensor(tails_batch), split_indices[:-1], torch.LongTensor(heads_batch)

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
        test_entity = ""
        rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                    header=None)
        ent_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                    header=None)

        attr_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'attr_embedding.tsv'), sep='\t',
                                     header=None)

        bias_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'bias_embedding.tsv'), sep='\t',
                                     header=None)

        relation_idx = 110
        print("relation label: ", self.idx2rel[relation_idx])
        rel = torch.tensor(rel_embedding.iloc[relation_idx].values).to(self.device)
        attr = torch.tensor(attr_embedding.iloc[relation_idx].values).to(self.device)
        bias = torch.tensor(bias_embedding.iloc[relation_idx].values).to(self.device)
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

    def filter_heads_by_numerical(self, numerical_operator, question_embedding, numerical_value):
        heads = self.all_species_indices
        predicted_attr = self.score_model.get_attribute_prediction(question_embedding)
        attr_batch = predicted_attr.repeat(len(heads), 1)
        numerical_values = self.score_model.get_numerical_prediction(heads, attr_batch)
        if numerical_operator == "larger":
            indices = (numerical_values > numerical_value)
            indices = indices.nonzero().squeeze(1)
        elif numerical_operator == "smaller":
            indices = (numerical_values < numerical_value)
            indices = indices.nonzero().squeeze(1)
        elif numerical_operator == "near":
            indices = torch.abs(numerical_values - numerical_value)
            _, indices = torch.topk(indices, k=10, largest=False)
        else:
            return heads.tolist()

        heads = heads[indices]
        return heads.tolist()

    def find_answers(self, question: str):
        """
        :param head_name: the standard name of the head entity
        :param k: number of results to return
        :param question: question in string format
        :param head_entity: the CID string for the head entity
        :return: score of all candidate answers
        """
        question = question.replace("'s ", " # ")
        numerical_value, numerical_string = self.numerical_value_extractor(question=question)
        question = question.replace(numerical_string, "")

        # 1. predict the numerical operators, if none, don't do numerical prediction, use normal <h,r,t> prediction
        _, tokenized_question = self.tokenize_question(question=question, repeat_num=1)
        single_question_embedding = self.score_model.get_question_embedding(question=tokenized_question)
        predicted_numerical_operator = self.score_model.get_numerical_operator(single_question_embedding)
        predicted_operator_idx = torch.argmax(predicted_numerical_operator).item()
        numerical_operator = self.operator_dict[predicted_operator_idx]

        if numerical_operator == "none":
            # TODO: do normal <h,r,t> prediction
            pass
        else:
            # TODO: filter heads by numerical values and operator
            START_TIME = time.time()
            filtered_heads = self.filter_heads_by_numerical(numerical_operator=numerical_operator,
                                                            question_embedding=single_question_embedding,
                                                            numerical_value=numerical_value)

            # TODO: prepare the batch for <h,r,t> prediction
            prediction_batch, tails, split_indices, filtered_heads_tensor = self.prepare_prediction_batch_numerical(
                heads=filtered_heads,
                question=single_question_embedding)

            with no_grad():
                triple_scores = self.score_model.get_scores(prediction_batch)
                for sub_score_list, sub_tail_list, head in zip(torch.tensor_split(triple_scores, split_indices),
                                                               torch.tensor_split(tails, split_indices),
                                                               filtered_heads):
                    top_scores, indices_top_k = torch.topk(sub_score_list, k=2, largest=self.largest)
                    labels_top_k = [self.idx2entity[sub_tail_list[index.item()].item()] for index in indices_top_k][0]
                    numerical_value = self.value_lookup(labels_top_k)
                    if numerical_value != "NODE HAS NO VALUE":
                        print("====== Labels =======")
                        print(labels_top_k)
                        print(numerical_value)
                        print(top_scores)
                        print("from head: ", self.idx2entity[head])

            print("Time used: ", time.time() - START_TIME)
            print("=========================================")

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

    my_engine.find_answers("vapour pressure around 0.1")
