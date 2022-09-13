import os
import pickle
import time
from random import random, choice, sample

import numpy as np
import pandas as pd
import torch
from transformers import BertTokenizer

from Marie.Util.location import DATA_DIR

tokenizer = BertTokenizer.from_pretrained('bert-base-cased')


class Dataset(torch.utils.data.Dataset):
    def __init__(self, df, negative_rate=20, remove_eh=False):
        # TODO: load the entity2idx, rel2idx
        # entity2idx, from name to index
        # rel2idx, from name to index
        # df: question, e_h, e_t, label
        # put label == 0 to positive set, label == 1 to negative set
        # self.df = df # TODO: remember to drop the index
        self.df = df
        self.df_pos = df[df['score'] == 1]
        # self.df_neg = df[df['score'] == 0]
        self.max_length = 12
        self.neg_sample_num = negative_rate

        e2i_path = open(os.path.join(DATA_DIR, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_path)
        r2i_path = open(os.path.join(DATA_DIR, 'relation2idx.pkl'), 'rb')
        self.relation2idx = pickle.load(r2i_path)
        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.relation2idx.keys())
        print('total entity number')
        print(self.ent_num)
        print('total triple number', len(self.df_pos))

        ent_embed_path = os.path.join(DATA_DIR, 'ent_embedding.tsv')
        self.ent_embedding = pd.read_csv(ent_embed_path, sep='\t', header=None)
        self.all_entities = list(set(self.entity2idx.keys()))
        self.all_tails = [e for e in self.all_entities if '_' in e]
        # tokenized_question_pos =
        # tokenized_question_neg
        self.all_relations = list(set(self.relation2idx.keys()))
        self.fake_entity_mapping = self.create_fake_entity_mapping()

        self.remove_eh = remove_eh

    def create_fake_entity_mapping(self):
        fake_entity_mapping = {}
        for entity in self.all_tails:
            head = entity.split('_')[0]
            valid_tails = [head + '_' + r for r in self.all_relations]
            clean_subset = [c_t for c_t in valid_tails if c_t != entity]
            fake_entity_mapping[entity] = clean_subset

        return fake_entity_mapping

    def __len__(self):
        return len(self.df_pos)

    def tokenize_question(self, df_subset, eh=None):
        eh = None
        if eh is not None:

            return [tokenizer(text.replace(eh, ''),
                              padding='max_length', max_length=self.max_length, truncation=True,
                              return_tensors="pt") for text in df_subset]
        else:
            return [tokenizer(text,
                              padding='max_length', max_length=self.max_length, truncation=True,
                              return_tensors="pt") for text in df_subset]

    def get_embedding(self, row):
        tokenized_question_pos = self.tokenize_question(row['question'], eh=row['head'].values[0])[0]
        a_m_batch = torch.cat(self.neg_sample_num * [tokenized_question_pos['attention_mask']])
        i_i_batch = torch.cat(self.neg_sample_num * [tokenized_question_pos['input_ids']])

        q_batch = {'attention_mask': a_m_batch,
                   'input_ids': i_i_batch}

        e_t_pos = self.entity2idx[row['tail'].tolist()[0]]
        e_h_pos = self.entity2idx[row['head'].tolist()[0]]

        # collection of all positive tail labels
        all_fake_e_t_neg_labels = self.fake_entity_mapping[row['tail'].tolist()[0]]

        # ============================================================================
        # Get the negative indices
        e_t_neg = torch.LongTensor(sample([self.entity2idx[e_t_l] for e_t_l in all_fake_e_t_neg_labels
                                           if 'type' not in e_t_l], self.neg_sample_num))

        # ====================== repeat the tensors to match pos-neg sizes ===========
        # repeat the tensors, for the positive sample to match the negative sample

        e_h_pos = torch.LongTensor([e_h_pos]).repeat(self.neg_sample_num)
        e_t_pos = torch.LongTensor([e_t_pos]).repeat(self.neg_sample_num)

        # create the negative sample by replace the tail

        # the thing self.neg_num times

        # return tokenized_question, e_h, e_t
        return {'question': q_batch, 'e_h': e_h_pos, 'e_t': e_t_pos}, \
               {'question': q_batch, 'e_h': e_h_pos, 'e_t': e_t_neg}

    def __getitem__(self, idx):
        # TODO: return positive_set, negative_set, each contains (e_h, e_t, tokenized question)

        START_TIME = time.time()
        row_pos = self.df_pos.iloc[[idx]]
        # TODO: replace the motherfucking tail or head
        # we need more neg samples, say, 20

        pos_set, neg_set = self.get_embedding(row_pos)
        return pos_set, neg_set


if __name__ == '__main__':
    batch_size = 6
    df_path = os.path.join(DATA_DIR, 'question_set_full')
    df = pd.read_csv(df_path, sep='\t')
    df_train, df_test = np.split(df.sample(frac=1, random_state=42), [int(.8 * len(df))])

    train_set = Dataset(df_train)
    test_set = Dataset(df_test)
    train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=batch_size, shuffle=True)
    test_dataloader = torch.utils.data.DataLoader(test_set, batch_size=batch_size, shuffle=True)
