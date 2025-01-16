from pprint import pprint

import numpy as np
import torch
import transformers
from torch import nn, optim, FloatTensor, LongTensor, no_grad
import os
import pandas as pd
from torch.utils.data.dataset import Dataset as TorchDataset
from tqdm import tqdm
from transformers import BertModel, BertTokenizer, AdamW
from Marie.Util.CommonTools.NLPTools import NLPTools
from Marie.Util.location import TRAINING_DIR, DEPLOYMENT_DIR, DATA_DIR

# TODO: a Dataset class that provides question examples and their relation
# TODO: also provides a rel embedding

max_len = 12


class TransERelPredictionModel(nn.Module):

    def __init__(self, device=torch.device("cpu"), for_training=False, dataset_dir="CrossGraph/pubchem", dim=20,
                 mode="general", tokenizer_name="bert-base-uncased"):
        super(TransERelPredictionModel, self).__init__()
        self.mode = mode
        self.dim = dim
        self.dataset_dir = dataset_dir
        self.device = device
        # self.bert = BertModel.from_pretrained('bert-base-cased')
        self.bert = BertModel.from_pretrained(tokenizer_name)
        self.dropout = nn.Dropout(0.1)

        if mode == "agent":
            self.mid_1 = nn.Linear(768, 512)
            self.output_linear = nn.Linear(512, self.dim)


        else:
            self.linear = nn.Linear(768, self.dim)  # keep this model ...
            self.mid_2 = nn.Linear(512, self.dim)

        self.criterion = torch.nn.CosineEmbeddingLoss()
        self.for_training = for_training
        if self.for_training:
            self.model_dir = DATA_DIR
        else:
            self.model_dir = DATA_DIR

    def load_model(self, model_name):
        print(" - Loading pretrained BERT Mapping model")
        self.load_state_dict(
            torch.load(os.path.join(self.model_dir, self.dataset_dir, model_name), map_location=self.device))

    def distance(self, emb_1, emb_2):
        """
        Calculate the simple manhattan distance between emb_1 and emb_2
        :param emb_1: projected rel
        :param emb_2: original rel
        :return: distance
        """

        assert emb_1.shape[1] == emb_2.shape[1]
        distance = (emb_1 - emb_2).norm(p=1, dim=1)
        return distance

    def predict(self, question):
        with no_grad():
            input_ids = torch.reshape(question['input_ids'], (-1, max_len)).to(self.device)
            attention_mask = torch.reshape(question['attention_mask'], (-1, max_len)).to(self.device)
            pooled_output = self.bert(input_ids=input_ids,
                                      attention_mask=attention_mask,
                                      return_dict=False)[1].to(self.device)
            linear_output = self.dropout(pooled_output.to(self.device)).to(self.device)

            if self.mode == "agent":
                linear_output = self.mid_1(linear_output.to(self.device))
                # linear_output = self.mid_2(linear_output)
                # linear_output = self.mid_3(linear_output)
                # linear_output = self.mid_4(linear_output)
                # linear_output = self.mid_5(linear_output)
                # linear_output = self.mid_6(linear_output)
                # linear_output = self.output_linear(dropout_output.to(self.device)).to(self.device)
                linear_output = self.output_linear(linear_output).to(self.device)

                return linear_output
            else:
                linear_output = self.linear(linear_output.to(self.device)).to(self.device)
                return linear_output

    def forward(self, question, true_linear_output):
        input_ids = torch.reshape(question['input_ids'], (-1, max_len))
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1]

        linear_output = self.dropout(pooled_output)
        if self.mode == "agent":

            linear_output = self.mid_1(linear_output)
            # linear_output = self.mid_2(linear_output)
            # linear_output = self.mid_3(linear_output)
            # linear_output = self.mid_4(linear_output)
            # linear_output = self.mid_5(linear_output)
            # linear_output = self.mid_6(linear_output)
            # linear_output = self.output_linear(linear_output)
            linear_output = self.output_linear(linear_output)

            # distance_agent = self.distance(linear_agent, true_linear_agent)
            # distance_output = self.distance(linear_output, true_linear_output)
            target = torch.LongTensor([1] * len(true_linear_output)).to(self.device)
            # distance_output = self.criterion(linear_output, true_linear_output, target).to(self.device)
            distance_output = self.distance(linear_output, true_linear_output)
            # return distance_agent + distance_output, linear_output, linear_agent
            return distance_output, distance_output, linear_output, linear_output
        else:
            linear_output = self.linear(linear_output)
            distance = self.distance(linear_output, true_linear_output)
            '''
            Update the loss function to do a calculation eh + r = et, where r is the output of linear_output ... 
            If the fine-tuning of the current thing fails 
            '''
            return distance, linear_output


if __name__ == "__main__":
    my_model = TransERelPredictionModel(device=torch.device("cpu"),
                                        dataset_dir=os.path.join(DATA_DIR, "CrossGraph/agents"), dim=40,
                                        mode="agent")

    my_model.load_model("bert_ontoagent_improved")

    nlp_tool = NLPTools()
    _, tokenzied_question = nlp_tool.tokenize_question("heat capacity",
                                                       repeat_num=0)
    agent_emb, output_emb = my_model.predict(question=tokenzied_question)
