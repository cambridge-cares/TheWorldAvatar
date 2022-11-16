import os

import pandas as pd
from torch.utils.data.dataset import Dataset as TorchDataset
from transformers import BertTokenizer

from Marie.Util.location import DATA_DIR


class Dataset(TorchDataset):
    def __init__(self, df, embedding_path):
        """
        This dataset provides (question, head, tail, score (binary))
        :param df:
        """
        super(Dataset, self).__init__()
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        self.df = df
        self.max_len = 12
        self.tokenized_questions = [self.tokenizer(text,
                                                   padding='max_length', max_length=self.max_len, truncation=True,
                                                   return_tensors="pt") for text in self.df['question']]

        self.ent_embedding = pd.read_csv(os.path.join(DATA_DIR, embedding_path, 'ent_embedding.tsv'), sep='\t', header=None)
        self.rel_embedding = pd.read_csv(os.path.join(DATA_DIR, embedding_path, 'rel_embedding.tsv'), sep='\t', header=None)
        self.heads = self.df["head"].tolist()
        self.tails = self.df["tail"].tolist()
        self.questions = self.df['question'].tolist()
        self.y = self.df["rel"]

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        return self.tokenized_questions[idx], self.heads[idx], \
               self.tails[idx], self.y.iloc[idx]
