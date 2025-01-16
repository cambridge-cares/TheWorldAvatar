import torch
import os
import pandas as pd
from torch import Tensor
from torch.nn.functional import one_hot
from torch.utils.data.dataset import Dataset as TorchDataset
from transformers import BertTokenizer
from Marie.Util.location import DATA_DIR
from Marie.Util.CommonTools.FileLoader import FileLoader

# TODO: a Dataset class that provides question examples and their relation
# TODO: also provides a rel embedding

max_len = 12


class Dataset(TorchDataset):
    def __init__(self, df, dataset_dir, operator_dict={}):
        """
        This dataset provides a train/val set with tokenized question and the correspondent relation embedding
        :param df:
        """
        super(Dataset, self).__init__()
        self.dataset_dir = dataset_dir
        self.file_loader = FileLoader(os.path.join(DATA_DIR, dataset_dir))
        entity2idx, idx2entity, rel2idx, idx2rel = self.file_loader.load_index_files()
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        self.df = df
        self.max_len = 12
        self.filter_words = ["###", "@@@", "&&&", "***", "%%%", "$$$"]
        all_text = self.df["question"]
        for filter_word in self.filter_words:
            all_text = [text.replace(filter_word, "") for text in all_text]
        self.tokenized_questions = [self.tokenizer(text,
                                                   padding='max_length', max_length=self.max_len, truncation=True,
                                                   return_tensors="pt") for text in all_text]
        self.rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                         header=None)
        self.operator_dict = operator_dict
        # For non numerical embedding methods, check whether attr_embedding exist before loading the
        # numerical embeddings

        # If no numerical embeddings are provided, there is no operator dict, fill the operator column with
        # placeholder value "none".

        rel_list = []
        for rel in self.df["rel"].tolist():
            rel_list.append(rel2idx[rel.strip()])
        self.y_r = self.rel_embedding.iloc[rel_list].reset_index(drop=True)

        attr_path = os.path.join(DATA_DIR, self.dataset_dir, 'attr_embedding.tsv')
        if os.path.exists(os.path.join(DATA_DIR, self.dataset_dir, 'attr_embedding.tsv')):
            self.attr_embedding = pd.read_csv(attr_path, sep='\t',
                                              header=None)
            self.bias_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'bias_embedding.tsv'), sep='\t',
                                              header=None)
            self.operators = Tensor(
                [self.operator_dict[opr.strip()] for opr in self.df["numerical_operator"].tolist()]).to(
                torch.int64)
            self.operators = one_hot(self.operators, num_classes=len(self.operator_dict)).to(torch.float)
            self.y_a = self.attr_embedding.iloc[rel_list].reset_index(drop=True)
            self.y_b = self.bias_embedding.iloc[rel_list].reset_index(drop=True)

        else:
            # if the ontology embedding is numerical embedding, the attr and bias embedding will not be used
            # so populate the columns with placeholders
            self.y_a = self.y_r
            self.y_b = self.y_r

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
