import json
import os

import numpy as np
import pandas as pd
import torch
from torch.utils.data import DataLoader
from Marie.Util.location import DATA_DIR
from transformers import BertTokenizer
from ast import literal_eval


class CrossGraphDataset(torch.utils.data.Dataset):
    def __init__(self, df):
        """
        Returns question, score - domain pairs,
        :param df:
	    question head true_score true_domain fake_score fake_domain
        """
        self.max_len = 12
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        self.df = df
        self.questions = self.df['question']
        self.tokenized_questions = [self.tokenizer(text,
                                                   padding='max_length', max_length=self.max_len, truncation=True,
                                                   return_tensors="pt")
                                    for text in self.df['question']]

        self.true_score = self.df['true_score'].values.tolist()
        self.true_domain = self.df['true_domain'].values.tolist()

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        return self.true_score[idx], self.true_domain[idx], self.tokenized_questions[idx]


if __name__ == '__main__':
    df = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'cross_graph_alignment_training.tsv'), sep='\t')
    df_train, df_test = np.split(df.sample(frac=1, random_state=11), [int(.8 * len(df))])

    dataset_train = CrossGraphDataset(df_train)
    dataloader_train = DataLoader(dataset_train, batch_size=32, shuffle=True)

    for true_score, true_domain, question in dataset_train:
        print(true_score)
        print(torch.FloatTensor(json.loads(true_domain)))



        # print(question)
    # for true_answer, fake_answer, true_domain, fake_domain, question in dataloader_test:
    #     true_answer = torch.cat(true_answer)
    #     fake_answer = torch.cat(fake_answer)
    #     true_domain = torch.cat(true_domain)
    #     fake_domain = torch.cat(fake_domain)
    #     pos_triple = (question, true_answer, true_domain)  # question, score, domain
    #     neg_triple = (question, fake_answer, fake_domain)
