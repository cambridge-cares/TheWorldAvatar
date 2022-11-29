import torch
import os
import pandas as pd
from torch.utils.data.dataset import Dataset as TorchDataset
from transformers import BertTokenizer
from Marie.Util.location import DATA_DIR

# TODO: a Dataset class that provides question examples and their relation
# TODO: also provides a rel embedding

max_len = 12


class Dataset(TorchDataset):
    def __init__(self, df, dataset_dir):
        """
        This dataset provides a train/val set with tokenized question and the correspondent relation embedding
        :param df:
        """
        super(Dataset, self).__init__()
        self.dataset_dir = dataset_dir
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
        self.df = df
        self.max_len = 12
        self.tokenized_questions = [self.tokenizer(text,
                                                   padding='max_length', max_length=self.max_len, truncation=True,
                                                   return_tensors="pt") for text in self.df.iloc[:, 0]]
        self.rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                         header=None)
        self.y = self.rel_embedding.iloc[self.df["rel"].tolist()].reset_index(drop=True)
        self.dim = self.rel_embedding.shape[1]

    def classes(self):
        return self.y

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        return self.tokenized_questions[idx], torch.FloatTensor(self.y.iloc[idx])
