import os
import pickle
import time
from random import random, choice, sample

import pandas as pd
import torch
from transformers import BertTokenizer

from Marie.Util.location import DATASET_DIR, DATA_DIR, EMBEDDING_DIR

tokenizer = BertTokenizer.from_pretrained('bert-base-cased')


class Dataset(torch.utils.data.Dataset):
    def __init__(self, df):
        # TODO: load the entity2idx, rel2idx
        # entity2idx, from name to index
        # rel2idx, from name to index
        # df: question, e_h, e_t, label
        # put label == 0 to positive set, label == 1 to negative set
        # self.df = df # TODO: remember to drop the index
        self.df = df
        self.df_pos = df[df['score'] == 1]
        self.df_neg = df[df['score'] == 0]
        self.max_length = 12
        e2i_path = open(os.path.join(DATA_DIR, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_path)
        r2i_path = open(os.path.join(DATA_DIR, 'relation2idx.pkl'), 'rb')
        self.relation2idx = pickle.load(r2i_path)
        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.relation2idx.keys())

        ent_embed_path = os.path.join(DATA_DIR, 'ent_embedding.tsv')
        self.ent_embedding = pd.read_csv(ent_embed_path, sep='\t')
        self.all_entities = list(set(self.entity2idx.keys()))
        self.all_tails = [e for e in self.all_entities if '_' in e]
        # tokenized_question_pos =
        # tokenized_question_neg
        self.all_relations = list(set(self.relation2idx.keys()))
        self.fake_entity_mapping = self.create_fake_entity_mapping()

    def create_fake_entity_mapping(self):
        fake_entity_mapping = {}
        for entity in self.all_tails:
            head = entity.split('_')[0]
            valid_tails = [head + '_' + r for r in self.all_relations]
            clean_subset = valid_tails
            fake_entity_mapping[entity] = clean_subset
        print('writing the file')
        with open('fake_entity_mapping.pickle', 'wb') as handle:
            pickle.dump(fake_entity_mapping, handle, protocol=pickle.HIGHEST_PROTOCOL)
        return fake_entity_mapping

    def __len__(self):
        return len(self.df_pos)

    def tokenize_question(self, df_subset):
        return [tokenizer(text,
                          padding='max_length', max_length=self.max_length, truncation=True,
                          return_tensors="pt") for text in df_subset]

    def get_embedding(self, row):
        tokenized_question = self.tokenize_question(row['question'])[0]
        e_t = self.entity2idx[row['tail'].tolist()[0]]
        e_h = self.entity2idx[row['head'].tolist()[0]]

        return tokenized_question, e_h, e_t

    def __getitem__(self, idx):
        # TODO: return positive_set, negative_set, each contains (e_h, e_t, tokenized question)

        START_TIME = time.time()
        row_pos = self.df_pos.iloc[[idx]]
        # TODO: replace the motherfucking tail or head
        tokenized_question_pos, e_h_pos, e_t_pos = self.get_embedding(row_pos)
        start_time_2 = time.time()
        fake_entity = choice(self.fake_entity_mapping[row_pos['tail'].tolist()[0]])
        fake_dict = choice(({'e_t': fake_entity}, {'e_h': fake_entity}))
        row_neg = self.df_pos.iloc[[idx]].replace(fake_dict)
        tokenized_question_neg, e_h_neg, e_t_neg = self.get_embedding(row_neg)
        return {'question': tokenized_question_pos, 'e_h': e_h_pos, 'e_t': e_t_pos}, \
               {'question': tokenized_question_neg, 'e_h': e_h_neg, 'e_t': e_t_neg}
