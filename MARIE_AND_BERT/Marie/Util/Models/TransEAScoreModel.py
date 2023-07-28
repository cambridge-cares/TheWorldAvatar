import time
from pprint import pprint

import numpy as np
import torch
import transformers
from torch import nn, optim, FloatTensor, LongTensor, no_grad, matmul
import os
import pandas as pd
from torch.nn.functional import normalize
from torch.utils.data.dataset import Dataset as TorchDataset
from tqdm import tqdm
from transformers import BertModel, BertTokenizer, AdamW

from Marie.Util.location import TRAINING_DIR, DEPLOYMENT_DIR, DATA_DIR

# TODO: a Dataset class that provides question examples and their relation
# TODO: also provides a rel embedding

max_len = 12


def numerical_operator_loss(predicted_operator, y_o):
    return nn.BCEWithLogitsLoss()(predicted_operator, y_o)


class TransEAScoreModel(nn.Module):
    """
        TransEA Rel Prediction predicts both relation and attribute embeddings .
    """

    def __init__(self, device=torch.device("cpu"), for_training=False, dataset_dir="CrossGraph/pubchem", dim=20,
                 output_dim=3):
        super(TransEAScoreModel, self).__init__()
        self.dim = dim
        self.dataset_dir = dataset_dir
        self.device = device
        self.bert = BertModel.from_pretrained('bert-base-cased')
        self.dropout = nn.Dropout(0)
        self.linear_R = nn.Linear(768, self.dim)  # keep this model ...
        self.linear_A = nn.Linear(768, self.dim)  # keep this model ...
        self.linear_B = nn.Linear(768, 1)  # keep this model ...
        self.linear_O_1 = nn.Linear(768, 512)
        self.linear_O_2 = nn.Linear(512, 256)
        self.linear_O_3 = nn.Linear(256, output_dim)
        self.relu = nn.ReLU()
        # self.linear_O_4 = nn.Linear(512, output_dim)
        self.sigmoid = nn.Sigmoid()
        self.norm = 1

        self.alpha = 0.5
        self.criterion = torch.nn.CosineEmbeddingLoss()
        self.for_training = for_training
        if self.for_training:
            self.model_dir = DATA_DIR
        else:
            self.model_dir = DATA_DIR

        self.ent_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                         header=None)

        self.rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                         header=None)

        self.attr_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'attr_embedding.tsv'), sep='\t',
                                          header=None)

        self.bias_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'bias_embedding.tsv'), sep='\t',
                                          header=None)

        proj_matrix_path = os.path.join(DATA_DIR, self.dataset_dir, 'proj_matrix.tsv')
        if os.path.exists(proj_matrix_path):
            self.proj_matrix = pd.read_csv(proj_matrix_path, sep='\t', header=None)

        self.cosine_similarity = torch.nn.CosineSimilarity(dim=1, eps=1e-08)

    def load_model(self, model_name):
        model_path = os.path.join(self.model_dir, self.dataset_dir, model_name)
        print(f" - Loading pretrained BERT Mapping model for {model_path}")
        self.load_state_dict(
            torch.load(model_path, map_location=self.device))

    def distance(self, emb_1, emb_2, emb_3, emb_4):
        """
        Calculate the simple manhattan distance between emb_1 and emb_2
        :param emb_1: projected rel
        :param emb_2: original rel
        :param emb_3: projected attribute
        :param emb_4: original attribute
        :return: distance_R and distance_A
        """
        assert emb_1.shape[1] == emb_2.shape[1]
        distance_R = (emb_1 - emb_2).norm(p=1, dim=1)
        assert emb_3.shape[1] == emb_4.shape[1]
        distance_A = (emb_3 - emb_4).norm(p=1, dim=1)
        return torch.abs(distance_R), torch.abs(distance_A)

    def triple_distance_transR(self, head, tail, projected_rel, relation_idx):
        b_size = len(head)
        relation_idx = [relation_idx] * b_size
        # reshape the proj_mat according to the batch_size
        projected_rel = torch.tensor(self.rel_embedding.iloc[relation_idx].values).to(self.device)
        proj_mat = torch.tensor(self.proj_matrix.iloc[relation_idx].values).to(self.device)
        proj_mat = proj_mat.view(b_size, self.dim, self.dim).to(self.device)
        # project e_h and e_t with proj_mat
        projected_e_h = self.project(ent=head, proj_mat=proj_mat).to(self.device)
        projected_e_t = self.project(ent=tail, proj_mat=proj_mat).to(self.device)

        # use l2 dissimilarity

        return (projected_e_h + projected_rel - projected_e_t).norm(p=2, dim=-1).to(self.device)

    def project(self, ent, proj_mat):
        proj_e = matmul(proj_mat, ent.view(-1, self.dim, 1))
        return proj_e.view(-1, self.dim)

    def triple_distance(self, head, tail, projected_rel):
        return (head + projected_rel - tail).norm(p=self.norm, dim=1).to(self.device)

    def get_scores(self, triplets, mode="transe"):
        with no_grad():
            e_h_idx = triplets['e_h'].reshape(-1, 1).squeeze(1).cpu()
            e_t_idx = triplets['e_t'].reshape(-1, 1).squeeze(1).cpu()
            question_single = triplets['single_question']
            question = question_single.repeat(len(e_h_idx), 1).to(self.device)
            head = torch.tensor(self.ent_embedding.iloc[e_h_idx].values).to(self.device)
            tail = torch.tensor(self.ent_embedding.iloc[e_t_idx].values).to(self.device)
            projected_rel, predicted_rel_idx = self.get_relation_prediction(question_embedding=question)
            if mode == "transe":
                triple_score = self.triple_distance(head=head, tail=tail, projected_rel=projected_rel)
            elif mode == "transr":
                triple_score = self.triple_distance_transR(head=head, tail=tail, projected_rel=projected_rel,
                                                           relation_idx=predicted_rel_idx)
            else:
                triple_score = self.triple_distance(head=head, tail=tail, projected_rel=projected_rel)

            return triple_score, predicted_rel_idx

    def get_numerical_operator(self, question_embedding):
        numerical_operator_output = self.linear_O_1(question_embedding).to(self.device)
        # numerical_operator_output = self.relu(numerical_operator_output)
        numerical_operator_output = self.linear_O_2(numerical_operator_output).to(self.device)
        # numerical_operator_output = self.relu(numerical_operator_output)
        numerical_operator_output = self.linear_O_3(numerical_operator_output).to(self.device)
        # numerical_operator_output = self.relu(numerical_operator_output)
        # numerical_operator_output = self.linear_O_4(numerical_operator_output).to(self.device)
        # numerical_operator_output = self.sigmoid(numerical_operator_output).to(self.device)
        return numerical_operator_output

    def get_question_embedding(self, question):
        input_ids = torch.reshape(question['input_ids'], (-1, max_len))
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1]

        question_embedding = self.dropout(pooled_output)
        return question_embedding

    def get_bias_prediction(self, question_embedding):
        """
        get the predicted embedding of the bias
        :param question_embedding:
        :return:
        """
        linear_output_B = self.linear_B(question_embedding.to(self.device)).to(self.device)
        return linear_output_B

    def get_numerical_prediction(self, head, attr, bias=None):
        """
            with heads and predicted attributes, predicts the numerical value
        """
        # find the closest embedding among all attr and get its bias ...
        #
        all_attr = torch.tensor(self.attr_embedding.values).to(self.device)
        all_bias = torch.tensor(self.bias_embedding.values).to(self.device)
        pred_attr = attr[0].repeat(len(all_attr), 1)
        cos_similarity = self.cosine_similarity(pred_attr, all_attr)
        best_match_idx = torch.argmax(cos_similarity).item()
        best_match_attr = all_attr[best_match_idx]
        best_match_bias = all_bias[best_match_idx]
        head = torch.tensor(self.ent_embedding.iloc[head].values).to(self.device)
        # attr_batch = best_match_attr.repeat(len(head), 1)
        bias_batch = best_match_bias.repeat(len(head), 1)
        value = (torch.sum(head * best_match_attr, dim=1) + bias_batch)
        return value[0], best_match_idx
        # return torch.sum(head * attr, dim=1) + bias

    def get_attribute_prediction(self, question_embedding):
        """
        Tokenized question, get the relation embedding
        """
        linear_output_A = self.linear_A(question_embedding.to(self.device)).to(self.device)
        return linear_output_A

    def get_relation_prediction(self, question_embedding):
        """
        Tokenized question, get the relation embedding
        """
        linear_output_R = self.linear_R(question_embedding.to(self.device)).to(self.device)

        all_rels = torch.tensor(self.rel_embedding.values).to(self.device)
        pred_rel = linear_output_R[0].repeat(len(all_rels), 1)
        cos_similarity = self.cosine_similarity(pred_rel, all_rels)
        best_match_idx = torch.argmax(cos_similarity).item()
        best_match_rel = all_rels[best_match_idx]
        return best_match_rel, best_match_idx
        # return linear_output_R

    def predict(self, question):
        """
        translate the numerical operator inside the model
        with the numerical value, numerical operator, the head entities, the candidate list
        calculate the scores of candidate answers
        return the numerical operator, numerical predictions, the scores of relation ...
        """

        with no_grad():
            input_ids = torch.reshape(question['input_ids'], (-1, max_len)).to(self.device)
            attention_mask = torch.reshape(question['attention_mask'], (-1, max_len)).to(self.device)
            pooled_output = self.bert(input_ids=input_ids,
                                      attention_mask=attention_mask,
                                      return_dict=False)[1].to(self.device)
            dropout_output = self.dropout(pooled_output.to(self.device)).to(self.device)
            linear_output_R = self.linear_R(dropout_output.to(self.device)).to(self.device)
            linear_output_A = self.linear_A(dropout_output.to(self.device)).to(self.device)
            numerical_operator_output = self.linear_O_1(dropout_output).to(self.device)
            # numerical_operator_output = self.relu(numerical_operator_output)
            numerical_operator_output = self.linear_O_2(numerical_operator_output).to(self.device)
            # numerical_operator_output = self.relu(numerical_operator_output)
            numerical_operator_output = self.linear_O_3(numerical_operator_output).to(self.device)
            # numerical_operator_output = self.relu(numerical_operator_output)
            # numerical_operator_output = self.linear_O_4(numerical_operator_output).to(self.device)
            # numerical_operator_output = self.sigmoid(numerical_operator_output).to(self.device)

            return linear_output_R, linear_output_A, numerical_operator_output

    def forward(self, question, y_r, y_a, y_o, y_b):
        input_ids = torch.reshape(question['input_ids'], (-1, max_len))
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1]

        dropout_output = self.dropout(pooled_output)
        linear_output_R = self.linear_R(dropout_output)
        linear_output_A = self.linear_A(dropout_output)
        linear_output_B = self.linear_B(dropout_output)

        numerical_operator_output = self.linear_O_1(dropout_output)
        # numerical_operator_output = self.relu(numerical_operator_output)
        numerical_operator_output = self.linear_O_2(numerical_operator_output)
        # numerical_operator_output = self.relu(numerical_operator_output)
        numerical_operator_output = self.linear_O_3(numerical_operator_output)
        # numerical_operator_output = self.relu(numerical_operator_output)
        # numerical_operator_output = self.linear_O_4(numerical_operator_output)
        # numerical_operator_output = self.sigmoid(numerical_operator_output)
        operator_loss = numerical_operator_loss(numerical_operator_output, y_o)

        distance_R, distance_A = self.distance(linear_output_R, y_r, linear_output_A, y_a)
        distance_B = torch.abs(y_b - linear_output_B)
        # distance = self.alpha * distance_R + self.alpha * distance_A + 8 * operator_loss + 0.4 * distance_B
        # distance = distance_R + distance_A + 10 * operator_loss + distance_B
        # distance = 200 * operator_loss
        distance = operator_loss
        # distance = (1 - self.alpha) * distance_R + self.alpha * distance_A + self.alpha * operator_loss
        distance_P = 5 * distance_R + 5 * distance_A + 0.1 * distance_B
        '''
        Update the loss function to do a calculation eh + r = et, where r is the output of linear_output ... 
        If the fine-tuning of the current thing fails 
        '''
        return distance, linear_output_R, linear_output_A, numerical_operator_output, linear_output_B, distance_P
