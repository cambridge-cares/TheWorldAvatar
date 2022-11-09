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

        self.question_repeated = []
        for ele in self.tokenized_questions:
            attention_mask_list = []
            input_ids_list = []
            for i in range(5):
                attention_mask = ele['attention_mask']
                input_ids = ele['input_ids']
                attention_mask_list.append(attention_mask)
                input_ids_list.append(input_ids)
                self.question_repeated.append({"attention_mask": attention_mask, "input_ids": input_ids})

        self.df.true_score = df.true_score.apply(literal_eval)
        self.df.fake_score = df.fake_score.apply(literal_eval)
        self.df.true_domain = df.true_domain.apply(literal_eval)
        self.df.fake_domain = df.fake_domain.apply(literal_eval)

        self.true_score = self.df['true_score'].values.tolist()
        self.fake_score = self.df['fake_score'].values.tolist()
        self.true_domain = self.df['true_domain'].values.tolist()
        self.fake_domain = self.df['fake_domain'].values.tolist()

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        return self.true_score[idx], self.fake_score[idx], \
               self.true_domain[idx], self.fake_domain[idx], self.question_repeated[idx]


if __name__ == '__main__':
    df = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'cross_graph_pairs.tsv'), sep='\t')
    df_train, df_test = np.split(df.sample(frac=1, random_state=11), [int(.8 * len(df))])

    dataset_test = CrossGraphDataset(df_test)
    dataloader_test = DataLoader(dataset_test, batch_size=32, shuffle=True)
    for true_answer, fake_answer, true_domain, fake_domain, question in dataloader_test:
        true_answer = torch.cat(true_answer)
        fake_answer = torch.cat(fake_answer)
        true_domain = torch.cat(true_domain)
        fake_domain = torch.cat(fake_domain)
        pos_triple = (question, true_answer, true_domain)  # question, score, domain
        neg_triple = (question, fake_answer, fake_domain)
