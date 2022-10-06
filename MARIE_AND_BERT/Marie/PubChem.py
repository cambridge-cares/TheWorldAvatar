import json
import os
import pickle
import time
import sys
from datetime import datetime as dt

sys.path.append("..")
import torch
from transformers import BertTokenizer
import logging
from Marie.SubgraphExtraction.SubgraphExtractor import SubgraphExtractor
from Marie.Util.Models.ModelBuilder_Score_Pretrained_BERT_removed_eh import ScoreModel
from Marie.Util.location import DEPLOYMENT_DIR, DATA_DIR
from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
logging.basicConfig(level=logging.DEBUG, filename='marie.log', format='%(asctime)s %(levelname)s:%(message)s')


class PubChemEngine:

    def __init__(self):
        self.subgraph_extractor = SubgraphExtractor(dataset_name='pubchem10000')
        self.chemical_nel = ChemicalNEL()
        assert (self.chemical_nel.find_cid("CO2")[1] == "CID280")
        print("1. Done initializing NEL ")
        '''Find the device available for running the model'''
        # use_cuda = torch.cuda.is_available()
        # self.device = torch.device("cuda" if use_cuda else "cpu")
        self.device = torch.device("cpu")
        '''Load pickles for idx - label and label - idx transformation '''
        i2e_file = open(os.path.join(DEPLOYMENT_DIR, 'idx2entity.pkl'), 'rb')
        self.idx2entity = pickle.load(i2e_file)
        e2i_file = open(os.path.join(DEPLOYMENT_DIR, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_file)

        print(f'=========== USING {self.device} ===============')
        '''Initialize the scoring model'''
        self.score_model_name = 'bert_embedding_10000'
        self.score_model = ScoreModel(device=self.device, model_name=self.score_model_name)
        self.score_model = self.score_model.to(self.device)
        '''Initialize tokenizer'''
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
        self.max_length = 12
        self.confidence_factor = 2
        print("2. Done initializing Scoring model ")

        # self.value_dict = json.loads(os.path.join())

    def value_lookup(self, node_name):
        return self.subgraph_extractor.value_lookup(node_name=node_name)

    def prepare_prediction_batch(self, question, head_entity, candidate_entities):
        """
        :param question: question in text
        :param head_entity: head entity index
        :param candidate_entities: list of candidate entity index
        :return: Ranked list of candidate entities
        """
        print(" - preparing prediction batch")
        candidate_entities = torch.LongTensor(candidate_entities).to(self.device)
        print(" - step 1")
        repeat_num = len(candidate_entities)
        print(" - step 2")
        tokenized_question_batch = self.tokenize_question(question, repeat_num)
        print(" - step 3")
        head_entity_batch = torch.LongTensor([head_entity]).repeat(repeat_num).to(self.device)
        print(" - step 4")
        prediction_batch = {'question': tokenized_question_batch, 'e_h': head_entity_batch, 'e_t': candidate_entities}
        print(" - step 5")
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
        return {'attention_mask': attention_mask_batch, 'input_ids': input_ids_batch}

    def find_answers(self, question, head_entity, k=5):
        """
        :param question: question in string format
        :param head_entity: the CID string for the head entity
        :return: score of all candidate answers
        """
        candidates = self.subgraph_extractor.retrieve_subgraph(head_entity)
        print("candidates:", candidates)
        pred_batch = self.prepare_prediction_batch(question, self.entity2idx[head_entity], candidates)
        scores = self.score_model.predict(pred_batch).cpu()
        print("prediction scores", scores)
        _, indices_top_k = torch.topk(scores, k=k, largest=False)
        labels_top_k = [self.idx2entity[candidates[index]] for index in indices_top_k]
        scores_top_k = [scores[index].item() for index in indices_top_k]
        return labels_top_k, scores_top_k

    def extract_head_ent(self, _question):
        print("extracting head entity")
        return self.chemical_nel.find_cid(question=_question)

    def process_answer(self, answer_list, nel_confidence, mention_string, name, score_list):
        result_list = []
        for answer, score in zip(answer_list, score_list):
            print('The answer:\t\t', answer)
            print('NEL confidence:\t\t', nel_confidence)
            print('Mentioned string:\t\t', mention_string)
            print('Name in Dictionary:\t\t', name)
            combined_confidence = round(min(1, nel_confidence * (1 / score) * self.confidence_factor), 2)
            print('Confidence:\t\t', combined_confidence)
            print('=====================================================')
            row = {"answer": self.value_lookup(answer), "from node": answer, "mention": mention_string,
                   "name": name, "confidence": combined_confidence}
            result_list.append(row)
        return result_list

    def run(self, question):
        """
        :param question:
        :return:
        """
        # get the mention,
        try:
            nel_confidence, cid, mention_string, name = self.extract_head_ent(question)
        except TypeError:
            print("Entity not recognized")
            logging.error(f"Error - Could not recognise any target from the question: {question}")
            return {"Error": "No target can be recognised from this question"}

        question = self.remove_head_entity(question, mention_string)
        answer_list, score_list = self.find_answers(question=question, head_entity=cid)
        return self.process_answer(answer_list, nel_confidence, mention_string, name, score_list)

    def remove_head_entity(self, _question, _head_entity):
        return _question.replace(_head_entity, '').strip()

    def self_inspection(self):
        wrong_questions = []
        test_questions = json.loads(open(os.path.join(DATA_DIR, 'Test/test_question_and_answers.json')).read())
        for question, target_answer in test_questions.items():
            answer = self.run(question=question)[0]
            print(answer)
            print('=========')
            print(target_answer)
            if answer != target_answer:
                tmp = {"question": question, "answer": answer, "target_answer": target_answer}
                wrong_questions.append(json.dumps(tmp, indent=4))

        if len(wrong_questions) == 0:
            return "<h2>The test is fully passed!</h2> <br/>" + str(dt.now())
        else:
            return f"<h2>The test failed! {len(wrong_questions)} out of {len(test_questions)} </h2> <br/> The failed questions are: <br/>" + "<hr/><br/> - ".join(wrong_questions)


if __name__ == '__main__':
    my_pubchem_engine = PubChemEngine()
    START_TIME = time.time()
    question = 'what is the exact mass of C6H6'
    # question_list = open(os.path.join(DATA_DIR, 'Test/test_questions.txt')).readlines()
    # for question in question_list:
        # print(question)
    answer = my_pubchem_engine.run(question)
    print(answer)
    print(time.time() - START_TIME)
