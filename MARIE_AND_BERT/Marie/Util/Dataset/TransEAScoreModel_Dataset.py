import torch
import os
import pandas as pd
from torch import Tensor
from torch.nn.functional import one_hot
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
        self.filter_words = ["###", "@@@", "&&&", "***", "%%%", "$$$"]
        all_text =  self.df.iloc[:, 0]
        for filter_word in self.filter_words:    
            all_text = [text.replace(filter_word, "") for text in all_text]
        self.tokenized_questions = [self.tokenizer(text,
                                                   padding='max_length', max_length=self.max_len, truncation=True,
                                                   return_tensors="pt") for text in all_text]
        self.rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                         header=None)
        self.attr_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'attr_embedding.tsv'), sep='\t',
                                          header=None)
                                          
        self.bias_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'bias_embedding.tsv'), sep='\t',
                                          header=None)

        self.operator_dict = {"smaller": 0, "larger": 1, "about": 2, "none": 3}

        self.operators = Tensor([self.operator_dict[opr] for opr in self.df["numerical_operator"].tolist()]).to(
            torch.int64)
        self.operators = one_hot(self.operators, num_classes=4).to(torch.float)
        self.y_r = self.rel_embedding.iloc[self.df["rel"].tolist()].reset_index(drop=True)
        self.y_a = self.attr_embedding.iloc[self.df["rel"].tolist()].reset_index(drop=True)
        self.y_b = self.bias_embedding.iloc[self.df["rel"].tolist()].reset_index(drop=True)

        self.dim = self.rel_embedding.shape[1]

    def classes(self):
        return self.y_r, self.y_a, self.operators

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        return self.tokenized_questions[idx], \
               torch.FloatTensor(self.y_r.iloc[idx]), \
               torch.FloatTensor(self.y_a.iloc[idx]), \
               self.operators[idx], torch.FloatTensor(self.y_b.iloc[idx])
