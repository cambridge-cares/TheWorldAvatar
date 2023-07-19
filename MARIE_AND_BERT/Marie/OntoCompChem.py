# Version 1 comes with only answer, score = (question, head)
import json
import os,sys
import traceback

from Marie.EntityLinking.IRILookup import IRILookup
from Marie.Util.CommonTools.NLPTools import NLPTools

sys.path.append("..")
import pickle
import pandas as pd
import torch
from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR
from Marie.Util.Models.ComplexScoreModel import ComplexScoreModel
from Marie.Util.Logging import MarieLogger
from transformers import BertTokenizer

"""
Use the comp-pubchem dictionary, retrieve 
"""


class OntoCompChemEngine:
    def __init__(self, nel=None):
        self.nel = nel
        self.marie_logger = MarieLogger()
        self.nlp = NLPTools(tokenizer_name="bert-base-uncased")
        # self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        self.max_length = 12
        self.dataset_dir = os.path.join(DATA_DIR, 'CrossGraph/ontocompchem')
        self.subgraph_extractor = HopExtractor(dataset_dir=self.dataset_dir, dataset_name='ontocompchem')
        i2e_file = open(os.path.join(self.dataset_dir, 'idx2entity.pkl'), 'rb')
        self.idx2entity = pickle.load(i2e_file)
        e2i_file = open(os.path.join(self.dataset_dir, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_file)
        self.device = torch.device("cpu")
        self.ent_embedding = pd.read_csv(os.path.join(self.dataset_dir, 'ent_embedding.tsv'), sep='\t', header=None)
        self.rel_embedding = pd.read_csv(os.path.join(self.dataset_dir, 'rel_embedding.tsv'), sep='\t', header=None)
        self.score_model = ComplexScoreModel(device=self.device, ent_embedding=self.ent_embedding,
                                             rel_embedding=self.rel_embedding, for_training=True,
                                             idx2entity=self.subgraph_extractor.entity_labels, load_model=False,
                                             dataset_dir=self.dataset_dir,
                                             model_name='score_model_general')
        self.value_dictionary_path = os.path.join(DATA_DIR, self.dataset_dir, "ontocompchem_value_dict.json")
        self.value_dictionary = json.loads(open(self.value_dictionary_path).read())
        model_path = os.path.join(self.dataset_dir, 'score_model_general')
        print("model path", model_path)
        # self.chemical_nel = ChemicalNEL(dataset_name="ontocompchem")
        self.chemical_nel = IRILookup(dataset_name="ontocompchem", enable_class_ner=False, nel=self.nel)

        # self.score_model.load_pretrained_model(model_path)

    # def remove_head_entity(self, _question, _head_entity):
    #     return _question.replace(_head_entity, '').strip()

    def run(self, question, head=None, mention=None):
        if mention is None:
            return ["EMPTY"], [-999], ["EMPTY"]
        question = question.replace("'s ", " of ")
        try:
            nel_confidence, cid, mention_string, name = self.chemical_nel.find_cid(mention=mention)
            question = self.nlp.remove_head_entity(_question=question, _head_entity=mention_string)
            if head is not None:
                cid = head
            return self.find_answers(head_entity=cid, question=question, head_name=name)
        except TypeError:
            self.marie_logger.error(f"Error - Could not recognise any target from the question: "
                                    f"{question} from {__name__}.{self.run.__name__}")
            return ["EMPTY"], [-999], ["EMPTY"]
            # return {"Error": "No target can be recognised from this question"}


    def test(self):
        good_counter = 0
        bad_counter = 0
        df_test = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'ontochemistry_cross_score.tsv'), sep='\t')
        df_test = df_test.sample(frac=0.01)
        for idx, row in df_test.iterrows():
            question = row['question']
            head = row['head']
            answer = row['answer']
            labels, _, _ = self.find_answers(head, question)
            print("===============")
            print()
            if answer in labels:
                good_counter += 1
            else:
                bad_counter += 1

            print("good counter", good_counter)
            print("bad counter", bad_counter)

    def value_lookup(self, node):
        if node in self.value_dictionary:
            return self.value_dictionary[node]
        else:
            return "NODE HAS NO VALUE"

    def find_answers(self, head_entity, question, head_name):
        try:
            head = self.entity2idx[head_entity]
            candidate_entities = self.subgraph_extractor.extract_neighbour_from_idx(head)
            question, head, tails = self.prepare_prediction_batch(question=question, head_entity=head,
                                                                  candidate_entities=candidate_entities)
            scores = self.score_model.find_answers(question=question, head=head, tail=tails)
            k = min(5, len(tails))
            _, indices_top_k = torch.topk(scores, k=k, largest=True)
            labels_top_k = [self.idx2entity[tails[index].item()] for index in indices_top_k]
            labels_top_k = self.append_value(labels_top_k)
            # labels_top_k = [f"{self.value_dictionary[l]} | {l}" for l in labels_top_k if l in self.value_dictionary]
            score_top_k = [scores[index].item() for index in indices_top_k]
            target_top_k = [head_name] * k
            return labels_top_k, score_top_k, target_top_k
        except:
            self.marie_logger.error('The attempt to find answer failed')
            self.marie_logger.error(traceback.format_exc())
            # return traceback.format_exc()
            return ["EMPTY"], [-999], ["EMPTY"]
    # def tokenize_question(self, question, repeat_num):
    #     """
    #     :param question: question in text
    #     :param repeat_num:
    #     :return:
    #     """
    #     tokenized_question = self.tokenizer(question,
    #                                         padding='max_length', max_length=self.max_length, truncation=True,
    #                                         return_tensors="pt")
    #     attention_mask, input_ids = tokenized_question['attention_mask'], tokenized_question['input_ids']
    #     attention_mask_batch = attention_mask.repeat(repeat_num, 1).to(self.device)
    #     input_ids_batch = input_ids.repeat(repeat_num, 1).to(self.device)
    #     return {'attention_mask': attention_mask_batch, 'input_ids': input_ids_batch}

    def append_value(self, label_list):
        appended_value_list = []
        for label in label_list:
            if label in self.value_dictionary:
                appended_value_list.append(f"{self.value_dictionary[label]} | {label}")
            else:
                appended_value_list.append(label)
        return appended_value_list

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
        tokenized_question_batch, _ = self.nlp.tokenize_question(question, repeat_num)
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
    from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
    cn = ChemicalNEL()
    my_ontochemistry_engine = OntoCompChemEngine(nel=cn)
    rst = my_ontochemistry_engine.run(question="what is co2's geometry", mention="co2")
    print(rst)