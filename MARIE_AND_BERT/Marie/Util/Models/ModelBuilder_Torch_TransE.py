import sys

from torch.utils.data import Subset

sys.path.append('../../../')


import os
import pickle
import random

import numpy as np
import pandas as pd
import torch.utils.data
from torch import nn, optim
from torch.nn import MarginRankingLoss
from tqdm import tqdm
from transformers import BertModel, BertTokenizer

from Marie.Util.Models.TransE import TransE
from Marie.Util.Models.ScoringModel_Dataset import Dataset

from Marie.Util.location import DATASET_DIR, DATA_DIR, EMBEDDING_DIR


class ScoreModel(nn.Module):

    def __init__(self, ent_embedding, device, dropout=0.5):
        super(ScoreModel, self).__init__()
        # load the weights
        self.criterion = MarginRankingLoss()
        self.device = device
        # create a stack for projecting bert results
        self.bert_reduce_stack = nn.Sequential(
            nn.Linear(768, 50),
            nn.ReLU(),
            nn.Dropout(dropout),

            # nn.Linear(512, 256),
            # nn.ReLU(),
            # nn.Dropout(dropout),
            #
            # nn.Linear(256, 128),
            # nn.ReLU(),
            # nn.Dropout(dropout),
            #
            # nn.Linear(128, 50),
            # nn.ReLU(),
            # nn.Dropout(dropout)
        )

        self.bert = BertModel.from_pretrained('bert-base-cased')
        self.ent_embedding = ent_embedding

    def forward(self, positive_triplets, negative_triplets):
        """
        In middle, measures x1 = dist(positive), x2 = dist(negative), where x1 is minimized, x2 is maximized

        :return: loss function based on the positive and negative component of the triple scoring function
                MarginRankLoss
        """
        # ================= layer 1 ==================
        nlp_components_pos = positive_triplets['question']
        nlp_components_neg = negative_triplets['question']

        _, pooled_output_pos = self.bert(input_ids=nlp_components_pos["input_ids"].squeeze(1).to(self.device),
                                         attention_mask=nlp_components_pos["attention_mask"].to(self.device),
                                         return_dict=False)

        _, pooled_output_neg = self.bert(input_ids=nlp_components_neg["input_ids"].squeeze(1).to(self.device),
                                         attention_mask=nlp_components_neg["attention_mask"].to(self.device),
                                         return_dict=False)

        projected_rel_pos = self.bert_reduce_stack(pooled_output_pos)
        projected_rel_neg = self.bert_reduce_stack(pooled_output_neg)

        dist_positive = self.distance(positive_triplets, projected_rel_pos)
        dist_negative = self.distance(negative_triplets, projected_rel_neg)

        return self.loss(dist_positive, dist_negative)

    def loss(self, dist_postive, dist_negative):
        target = torch.tensor([-1], dtype=torch.long).to(self.device)  # set p = -1
        return self.criterion(dist_postive.to(self.device), dist_negative.to(self.device), target).to(self.device)

    def distance(self, triplets, projected_rel):
        e_h_idx = triplets['e_h']
        e_t_idx = triplets['e_t']
        head = torch.tensor(self.ent_embedding.iloc[e_h_idx].values).to(self.device)
        tail = torch.tensor(self.ent_embedding.iloc[e_t_idx].values).to(self.device)
        return (head + projected_rel - tail).norm(p=1, dim=1).to(self.device)

    def predict(self, triplet):
        nlp_components = triplet['question']
        _, pooled_output = self.bert(input_ids=nlp_components["input_ids"].squeeze(1).to(self.device),
                                     attention_mask=nlp_components["attention_mask"].to(self.device), return_dict=False)
        projected_rel = self.bert_reduce_stack(pooled_output)
        return self.distance(triplet, projected_rel).to(self.device)


class Trainer:

    def __init__(self):
        self.df_path = os.path.join(DATA_DIR, 'question_set_full')
        self.df = pd.read_csv(self.df_path, sep='\t')
        self.df_train, self.df_test = np.split(self.df.sample(frac=1, random_state=42), [int(.8 * len(self.df))])

        self.batch_size = 128
        self.epoches = 1000
        self.learning_rate = 5e-5
        self.step = 0
        self.test_frequency = 100
        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {self.device} ===============')

        self.train_set = Dataset(self.df_train)
        self.test_set = Dataset(self.df_test)
        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=self.batch_size, shuffle=True)

        self.ent_embedding = self.train_set.ent_embedding
        self.model = ScoreModel(self.ent_embedding, device=self.device)
        self.optimizer = optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.entity_labels = list(self.train_set.entity2idx.keys())

        num_train_examples = 100
        self.sample_ds = Subset(self.train_set, np.arange(num_train_examples))

    def hit_at_k(self, predictions, ground_truth_idx, k: int = 10):
        _, indices_top_k = torch.topk(predictions.to(self.device), k=k, largest=False)
        if ground_truth_idx in indices_top_k.to(self.device):
            return 1
        else:
            return 0

    def train(self):


        with tqdm(total=self.epoches, unit=' epoch') as tepoch:
            model = self.model.cuda()
            for epoch_num in range(self.epoches):
                tepoch.set_description(f'Epoch {epoch_num}')
                model.train()
                total_loss_train = 0
                for positive_set, negative_set in tqdm(self.train_dataloader):
                    self.optimizer.zero_grad()
                    loss = model(positive_set, negative_set)
                    loss.mean().backward()
                    loss = loss.data.cuda()
                    self.optimizer.step()
                    self.step += 1
                    total_loss_train += loss.mean().item()
                print(f'total_loss_train: {total_loss_train}')

                if epoch_num % self.test_frequency == 0:
                    self.evaluate()

    def evaluate(self):
        total_prediction_loss = 0
        hit_1_counter = 0
        hit_2_counter = 0
        total_case_counter = 0
        with torch.no_grad():
            for positive_set, _ in self.test_dataloader:
                total_prediction_loss += self.model.predict(positive_set).mean().item()
            val_batch_counter = 0

            for positive_triplet, _ in tqdm(self.sample_ds):
                val_batch_counter += 1
                e_h = positive_triplet['e_h']
                e_t = positive_triplet['e_t']
                question = positive_triplet['question']
                e_h_label = self.entity_labels[e_h]
                e_t_label = self.entity_labels[e_t]

                all_possible_e_t_idx = [self.entity_labels.index(e) for e in self.entity_labels
                                        if '_' in e and e.startswith(e_h_label + '_')]

                ground_truth_idx = all_possible_e_t_idx.index(e_t)
                triplet_num = len(all_possible_e_t_idx)

                e_h_batch = torch.tensor(e_h).repeat(triplet_num).type(torch.LongTensor)
                e_t_batch = torch.tensor(all_possible_e_t_idx).type(torch.LongTensor)

                a_m_batch = torch.cat(triplet_num * [question['attention_mask']])
                i_i_batch = torch.cat(triplet_num * [question['input_ids']])

                q_batch = {'attention_mask': a_m_batch,
                           'input_ids': i_i_batch}
                test_batch = {'question': q_batch, 'e_h': e_h_batch, 'e_t': e_t_batch}

                predictions = self.model.predict(test_batch)
                # _, indices_top_k = torch.topk(k=1, input=predictions, largest=False)

                total_case_counter += 1
                hit_1_counter += self.hit_at_k(predictions,ground_truth_idx,1)
                hit_2_counter += self.hit_at_k(predictions,ground_truth_idx,2 )

        print(f'hit_1 {hit_1_counter} out of {total_case_counter}, ratio: {hit_1_counter/total_case_counter}')
        print(f'hit_2 {hit_2_counter} out of {total_case_counter}, ratio: {hit_2_counter/total_case_counter}')
        print(f'total_prediction_loss: {total_prediction_loss}')


if __name__ == '__main__':
    trainer = Trainer()
    trainer.train()
