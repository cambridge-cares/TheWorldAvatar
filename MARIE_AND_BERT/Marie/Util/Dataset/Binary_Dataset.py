import os, sys
import pickle

import numpy as np
import pandas as pd
from transformers import BertTokenizer

sys.path.append('../../../')

import torch
from Marie.Util.location import DATA_DIR

tokenizer = BertTokenizer.from_pretrained('bert-base-cased')


class Dataset(torch.utils.data.Dataset):
    # only return one set this time, but provide 0,1 labels
    def __init__(self, df):
        self.df = df
        self.max_length = 12
        e2i_path = open(os.path.join(DATA_DIR, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_path)
        r2i_path = open(os.path.join(DATA_DIR, 'relation2idx.pkl'), 'rb')
        self.relation2idx = pickle.load(r2i_path)
        self.Y = self.df['score']
        ent_embed_path = os.path.join(DATA_DIR, 'ent_embedding.tsv')
        self.ent_embedding = pd.read_csv(ent_embed_path, sep='\t')


    def get_embedding(self, row):
        tokenized_question = self.tokenize_question(row['question'])[0]
        e_t = self.entity2idx[row['tail'].tolist()[0]]
        e_h = self.entity2idx[row['head'].tolist()[0]]

        return tokenized_question, e_h, e_t

    def tokenize_question(self, df_subset):
        return [tokenizer(text,
                          padding='max_length', max_length=self.max_length, truncation=True,
                          return_tensors="pt") for text in df_subset]

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        row = self.df.iloc[[idx]]
        tokenized_question, e_h, e_t = self.get_embedding(row)
        return {'question': tokenized_question, 'e_h': e_h, 'e_t': e_t}, 1 - self.Y.iloc[[idx]].tolist()[0]


if __name__ == '__main__':
    df_path = os.path.join(DATA_DIR, 'question_set_full')
    df = pd.read_csv(df_path, sep='\t')
    df_train, df_test = np.split(df.sample(frac=1, random_state=42), [int(.8 * len(df))])

    test_set = Dataset(df_test)
    test_dataloader = torch.utils.data.DataLoader(test_set, batch_size= 64, shuffle=True)
    for test_x, test_y in test_dataloader:
        print(test_y)


