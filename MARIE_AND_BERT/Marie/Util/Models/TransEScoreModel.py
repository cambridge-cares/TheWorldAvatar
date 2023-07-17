import sys

sys.path.append('../../../')

import os
import pandas as pd
import torch.utils.data
from torch import nn
from Marie.Util.location import DATA_DIR
from Marie.Util.Models.TransERelPredictionModel import TransERelPredictionModel
from Marie.Util.Logging import MarieLogger


class TransEScoreModel(nn.Module):

    def __init__(self, device, model_name, for_training=False, dataset_dir=None, dim=20, tokenizer_name = "bert-base-cased"):
        super(TransEScoreModel, self).__init__()
        self.logger = MarieLogger()
        self.dim = dim
        self.dataset_dir = dataset_dir
        self.device = device
        self.bert_with_reduction = TransERelPredictionModel(device=device, dataset_dir=self.dataset_dir, dim=self.dim, tokenizer_name=tokenizer_name)


        self.bert_with_reduction = self.bert_with_reduction.to(self.device)
        self.bert_with_reduction.load_model(model_name)
        if for_training:
            self.data_dir = DATA_DIR
        else:
            self.data_dir = DATA_DIR

        self.rel_embedding = pd.read_csv(os.path.join(self.data_dir, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                         header=None)
        self.ent_embedding = pd.read_csv(os.path.join(self.data_dir, self.dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                         header=None)

    def distance(self, triplets, projected_rel):
        e_h_idx = triplets['e_h'].reshape(-1, 1).squeeze(1).cpu()
        e_t_idx = triplets['e_t'].reshape(-1, 1).squeeze(1).cpu()
        head = torch.tensor(self.ent_embedding.iloc[e_h_idx].values).to(self.device)
        tail = torch.tensor(self.ent_embedding.iloc[e_t_idx].values).to(self.device)
        # TODO: make the embedding torch embeddings and freeze the embeddings

        return (head + projected_rel - tail).norm(p=1, dim=1).to(self.device)

    def predict(self, triplet):
        self.logger.info(" - predicting scores")
        nlp_components_pos = triplet['question']
        projected_rel_pos = self.bert_with_reduction.predict(nlp_components_pos)
        dist_positive = self.distance(triplet, projected_rel_pos)
        self.logger.info(" - Done predicting scores")
        return dist_positive
