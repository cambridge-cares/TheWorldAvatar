import os
import pickle
import time
from pprint import pprint

import torch
from transformers import BertTokenizer

from Marie.SubgraphExtraction.SubgraphExtractor import SubgraphExtractor
# from Marie.EntityLinking.Inference import NELInfer
from Marie.Util.Models.ModelBuilder_Score_Pretrained_BERT_removed_eh import ScoreModel
from Marie.Util.location import DATA_DIR


class PubChemEngine:

    def __init__(self):
        self.subgraph_extractor = SubgraphExtractor()
        # print('============ Initializing entity linking ============')
        # self.entity_linker = NELInfer('conf/base500.yaml')
        # print('============ Done initializing entity linking ==============')
        '''Find the device available for running the model'''
        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        i2e_file = open(os.path.join(DATA_DIR, 'idx2entity.pkl'), 'rb')
        self.idx2entity = pickle.load(i2e_file)

        print(f'=========== USING {self.device} ===============')
        '''Initialize the scoring model'''
        self.score_model_name = 'bert_model_embedding_20_cosine_single_layer_pubchem500_no_eh'
        self.score_model = ScoreModel(device=self.device, model_name=self.score_model_name)
        self.score_model = self.score_model.to(self.device)
        '''Initialize tokenizer'''
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
        self.max_length = 12

    def prepare_prediction_batch(self, question, head_entity, candidate_entities):
        '''
        :param question: question in text
        :param head_entity: head entity index
        :param candidate_entities: list of candidate entity index
        :return: Ranked list of candidate entities
        '''
        candidate_entities = torch.LongTensor(candidate_entities).to(self.device)
        repeat_num = len(candidate_entities)
        tokenized_question_batch = self.tokenize_question(question, repeat_num)
        head_entity_batch = torch.LongTensor([head_entity]).repeat(repeat_num).to(self.device)
        prediction_batch = {'question': tokenized_question_batch, 'e_h': head_entity_batch, 'e_t': candidate_entities}
        return prediction_batch

    def tokenize_question(self, question, repeat_num):
        '''
        :param question: question in text
        :param repeat_num:
        :return:
        '''
        tokenized_question = self.tokenizer(question,
                                            padding='max_length', max_length=self.max_length, truncation=True,
                                            return_tensors="pt")
        attention_mask, input_ids = tokenized_question['attention_mask'], tokenized_question['input_ids']
        attention_mask_batch = attention_mask.repeat(repeat_num, 1).to(self.device)
        input_ids_batch = input_ids.repeat(repeat_num, 1).to(self.device)
        return {'attention_mask': attention_mask_batch, 'input_ids': input_ids_batch}

    def find_answer(self, prediction_batch):
        return self.score_model.predict(prediction_batch)

    # def extract_head_ent(self, question):
    #     return self.entity_linker.infer([{"text": question}])


if __name__ == '__main__':
    my_pubchem_engine = PubChemEngine()
    START_TIME = time.time()
    candidates = my_pubchem_engine.subgraph_extractor.retrieve_subgraph('CID1')
    pred_batch = my_pubchem_engine.prepare_prediction_batch('what is the molecular mass of', 0, candidates)
    answers = my_pubchem_engine.find_answer(pred_batch).cpu()
    _, indices_top_k = torch.topk(answers, k=1, largest=False)
    first_answer = indices_top_k[0]
    answer_index = candidates[first_answer]
    answer_label = my_pubchem_engine.idx2entity[answer_index]
    print(answer_label)
    answer_value = None
    print(f'Took {time.time() - START_TIME} seconds')
