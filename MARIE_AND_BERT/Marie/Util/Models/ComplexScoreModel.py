import json
import sys
from torch.optim.lr_scheduler import ExponentialLR
import numpy as np
import torch
from torch import nn, no_grad
import os
from transformers import BertModel
sys.path.append("../../..")
from Marie.Util.location import TRAINING_DIR, DEPLOYMENT_DIR, DATA_DIR


class ComplexScoreModel(nn.Module):
    """
    The upgraded version of the prediction mapping model, which able to derive the implicit relation on the fly.
    Input: question, head, tail
    Output: e_q
    Training: score(head, predicted_rel, tail), compare the score to the real score ... true / fake
    """

    def __init__(self, ent_embedding=None, rel_embedding=None, for_training=False, device=torch.device("cpu"),
                 tail_rel_embedding=None, idx2entity=None, load_model=True, dataset_dir=None, model_name=None):
        super(ComplexScoreModel, self).__init__()

        self.max_len = 12
        self.device = device
        self.bert = BertModel.from_pretrained('bert-base-uncased')
        self.dropout = nn.Dropout(0)
        self.dropout_relation = nn.Dropout(0.1)
        self.linear = nn.Linear(768, 80)  # keep this model ...
        self.sigmoid = nn.Sigmoid()
        self.for_training = for_training
        if self.for_training:
            self.model_dir = TRAINING_DIR
        else:
            self.model_dir = DEPLOYMENT_DIR

        embedding_dim = int(ent_embedding.shape[1])
        half = int(embedding_dim / 2)
        self.ent_embedding = ent_embedding
        self.rel_embedding = rel_embedding
        self.re_ent = ent_embedding.iloc[:, :half]  # first half of the embedding
        self.im_ent = ent_embedding.iloc[:, half:]
        self.re_rel = rel_embedding.iloc[:, :half]
        self.im_rel = rel_embedding.iloc[:, half:]
        self.half = half
        # self.criterion = nn.BCELoss()
        self.tail_rel_embedding = tail_rel_embedding
        self.idx2entity = idx2entity
        self.load_model = load_model
        self.dataset_dir = dataset_dir
        self.model_name = model_name
        self.model_path = os.path.join(DATA_DIR, self.dataset_dir, self.model_name)

        if os.path.exists(self.model_path):
            self.load_state_dict(torch.load(self.model_path, map_location=self.device))
        # TODO: load the embeddings and divide them into two parts re and im

    # def load_pretrained_model(self, path):
        # print(f" - Loading pretrained BERT Mapping model from {path}")


    def pointwise_bce(self, preds, target):
        """
        :param preds: model predicted score of the triple
        :param target: true score of the triple, which is a binary value 0 or 1 ...
        :return: Binary Cross Enthropy with Logits loss (Built in sigmoid activation)
        """
        preds = torch.clamp(preds, min=0, max=1)
        loss = torch.nn.BCEWithLogitsLoss()(preds, target)
        return loss

    def score(self, triple, single=False):
        head_idx = triple[0]
        predicted_rel = triple[1]
        tail_idx = triple[2]
        re_head = torch.FloatTensor(self.re_ent.iloc[head_idx].values.tolist()).to(self.device)
        im_head = torch.FloatTensor(self.im_ent.iloc[head_idx].values.tolist()).to(self.device)

        if single:
            re_rel, im_rel = torch.split(predicted_rel, self.half)
        else:
            re_rel = predicted_rel[:, :self.half]
            im_rel = predicted_rel[:, self.half:]

        re_tail = torch.FloatTensor(self.re_ent.iloc[tail_idx].values.tolist()).to(self.device)
        im_tail = torch.FloatTensor(self.im_ent.iloc[tail_idx].values.tolist()).to(self.device)
        pred = - torch.sum(re_head * re_tail * re_rel + im_head * im_tail * re_rel +
                           re_head * im_tail * im_rel - im_head * re_tail * im_rel, -1).to(self.device)
        return pred

    def forward(self, question, head, tail, y):
        """
        :param question: tokenized question ({"attention_mask": [0,0,...,1]})
        :param head: idx of the head entity
        :param tail: idx of the tail entity
        :param y: the score of the triple, either 0 or 1
        :return:
        """
        input_ids = torch.reshape(question['input_ids'], (-1, self.max_len))
        attention_mask = torch.reshape(question['attention_mask'], (-1, self.max_len))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1].to(self.device)

        true_rel = torch.FloatTensor(self.rel_embedding.iloc[y].values.tolist()).to(self.device)
        dropout_output = self.dropout(pooled_output).to(self.device)
        predicted_rel = self.linear(dropout_output).to(self.device)
        triple_score = self.score((head, predicted_rel, tail)).to(self.device)
        distance = (predicted_rel - true_rel).norm(p=1, dim=1)
        return distance, triple_score

    def predict(self, question, head, tail, debug=False):
        input_ids = torch.reshape(question['input_ids'], (-1, self.max_len)).to(self.device)
        attention_mask = torch.reshape(question['attention_mask'], (-1, self.max_len)).to(self.device)
        pooled_output = self.bert(input_ids=input_ids,
                                  attention_mask=attention_mask,
                                  return_dict=False)[1].to(self.device)

        dropout_output = self.dropout(pooled_output).to(self.device)
        predicted_rel = self.linear(dropout_output).to(self.device)
        if debug:
            print("pooled_output", pooled_output[0])
            print("input_ids", input_ids[0])
            print("predicted rel", predicted_rel[0])

        triple_score = self.score((head, predicted_rel, tail)).to(self.device)
        return triple_score

    def find_answers(self, question, head, tail):
        with no_grad():
            return self.predict(question, head, tail)




