import datetime
import json
import sys
import time
from itertools import permutations

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

    def __init__(self, ent_embedding, device, dropout=0.1):
        super(ScoreModel, self).__init__()
        # load the weights
        self.criterion = MarginRankingLoss(margin=0)  # to make sure that the positive triplet always have smaller
        # distance than negative ones
        self.device = device
        # create a stack for projecting bert results
        self.bert_reduce_stack = nn.Sequential(
            nn.Linear(768, 512),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(512, 256),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(256, 128),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(128, 50),
            nn.ReLU(),
            nn.Dropout(dropout)
        )

        self.bert = BertModel.from_pretrained('bert-base-cased')
        self.ent_embedding = ent_embedding
        self.sigmoid = nn.Sigmoid()
        self.linear = nn.Linear(1, 1)

    def forward(self, positive_triplets, negative_triplets):
        """
        In middle, measures x1 = dist(positive), x2 = dist(negative), where x1 is minimized, x2 is maximized

        :return: loss function based on the positive and negative component of the triple scoring function
                MarginRankLoss
        """
        # ================= layer 1 ==================
        nlp_components_pos = positive_triplets['question']
        nlp_components_neg = negative_triplets['question']

        # print(nlp_components_pos['input_ids'].shape)

        # make it a 64 * 20 x 12, squeeze dim 1?
        # print(torch.reshape(nlp_components_pos['input_ids'], (-1, 12)).shape)

        input_ids_pos = torch.reshape(nlp_components_pos['input_ids'], (-1, 12)).to(self.device)
        attention_mask_pos = torch.reshape(nlp_components_pos["attention_mask"], (-1, 12)).to(self.device)

        _, pooled_output_pos = self.bert(input_ids=input_ids_pos,
                                         attention_mask=attention_mask_pos,
                                         return_dict=False)

        input_ids_neg = torch.reshape(nlp_components_neg['input_ids'], (-1, 12)).to(self.device)
        attention_mask_neg = torch.reshape(nlp_components_neg["attention_mask"], (-1, 12)).to(self.device)

        _, pooled_output_neg = self.bert(input_ids=input_ids_neg.to(self.device),
                                         attention_mask=attention_mask_neg.to(self.device),
                                         return_dict=False)

        projected_rel_pos = self.bert_reduce_stack(pooled_output_pos)
        projected_rel_neg = self.bert_reduce_stack(pooled_output_neg)

        # dist_positive = self.distance(positive_triplets, projected_rel_pos).unsqueeze(1).type(
        # torch.FloatTensor).to(self.device) dist_negative = self.distance(negative_triplets,
        # projected_rel_neg).unsqueeze(1).type(torch.FloatTensor).to(self.device)
        dist_positive = self.distance(positive_triplets, projected_rel_pos)
        dist_negative = self.distance(negative_triplets, projected_rel_neg)
        """
            Insert a linear layer before sigmoid to adjust the scale of the distance  
        """
        # l_pos = self.linear(dist_positive)
        # l_neg = self.linear(dist_negative)
        # TODO: give a sigmoid ... will that help?
        # logit_pos = self.sigmoid(l_pos)  # neg the distance for sigmoid, logit_pos assume to be 1
        # logit_neg = self.sigmoid(l_neg)  # neg the distance for sigmoid, neg assume to be 0

        """
            Assume that the ideal dist for pos is 0, for neg is a large integer ... p = -1 
            MarginRankingLoss max(0, -p * (x1 - x2)), with the assumption that x2 is larger than x1 
            
            max(0, (x1 - x2) + 1), if it is reversed, x1 = 1, x2 = 0, max(0, 2) = 2, x1 =1, x2 = 1, max(0,1) = 1
            
        """

        # return self.loss(logit_neg, logit_pos)
        return self.loss(dist_positive, dist_negative)

    def loss(self, dist_positive, dist_negative):
        target = torch.tensor([-1], dtype=torch.long).to(self.device)  # set p = -1
        return self.criterion(dist_positive.to(self.device), dist_negative.to(self.device), target).to(self.device)

    def distance(self, triplets, projected_rel):
        e_h_idx = triplets['e_h'].reshape(-1, 1).squeeze(1)
        e_t_idx = triplets['e_t'].reshape(-1, 1).squeeze(1)
        head = torch.tensor(self.ent_embedding.iloc[e_h_idx].values).to(self.device)
        tail = torch.tensor(self.ent_embedding.iloc[e_t_idx].values).to(self.device)
        return (head + projected_rel - tail).norm(p=1, dim=1).to(self.device)

    def predict(self, triplet):
        nlp_components = triplet['question']

        input_ids = torch.reshape(nlp_components['input_ids'], (-1, 12)).to(self.device)
        attention_mask = torch.reshape(nlp_components["attention_mask"], (-1, 12)).to(self.device)

        _, pooled_output = self.bert(input_ids=input_ids,
                                     attention_mask=attention_mask)
        projected_rel = self.bert_reduce_stack(pooled_output)
        predicted_distance = self.distance(triplet, projected_rel).to(self.device)
        return predicted_distance


class Trainer:

    def __init__(self, epoches=20, negative_rate=20, learning_rate=5e-4, drop_out=0.1):
        self.df_path = os.path.join(DATA_DIR, 'question_set_full')
        self.df = pd.read_csv(self.df_path, sep='\t')

        self.df = self.df.sample(frac=0.1)
        self.df.to_csv('sample_df.csv')

        self.df_train, self.df_test = np.split(self.df.sample(frac=1, random_state=42), [int(.8 * len(self.df))])

        self.batch_size = 8
        self.epoches = epoches
        self.learning_rate = learning_rate
        self.step = 0
        self.test_frequency = 1
        self.drop_out = drop_out

        self.neg_rate = negative_rate

        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {self.device} ===============')

        self.train_set = Dataset(self.df_train, negative_rate=self.neg_rate)
        self.test_set = Dataset(self.df_test, negative_rate=self.neg_rate)
        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=self.batch_size, shuffle=True)

        self.ent_embedding = self.train_set.ent_embedding
        self.model = ScoreModel(self.ent_embedding, device=self.device, dropout=self.drop_out)
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

        init_train_loss = 0
        init_val_loss = 0
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
                    total_loss_val = self.evaluate()

                if epoch_num == 0:
                    init_train_loss = total_loss_train
                    init_val_loss = total_loss_val

            return total_loss_train, total_loss_val, init_train_loss, init_val_loss

    def make_prediction_batch(self, pos_triplets, neg_triplets):
        """
        :param pos_triplets:
        :param neg_triplets:
        :return: a list of batches, each batch contains a list of questions beginning with the ground truth
        triplet. As a result, ideally, the predicted_truth_idx should be 0 ..
        """
        prediction_batch_list = []
        question = pos_triplets['question']
        # Insert the first element to the neg ...
        for e_t_neg_x, e_t_pos_x, e_h_pos_x, a_m_x, i_i_x in \
                zip(neg_triplets['e_t'], pos_triplets['e_t'],
                    pos_triplets['e_h'], question['attention_mask'],
                    question['input_ids']):
            prediction_batch = {}
            # repeat each of the elements to 21
            e_t_to_test = torch.cat([e_t_pos_x[0].unsqueeze(0), e_t_neg_x], dim=0)
            e_h_to_test = torch.cat([e_h_pos_x, e_h_pos_x[0].unsqueeze(0)], dim=0)
            a_m_to_test = torch.cat([a_m_x, a_m_x[0].unsqueeze(0)], dim=0)
            i_i_to_test = torch.cat([i_i_x, i_i_x[0].unsqueeze(0)], dim=0)
            prediction_batch['e_t'] = e_t_to_test
            prediction_batch['e_h'] = e_h_to_test
            prediction_batch['question'] = {}
            prediction_batch['question']['attention_mask'] = a_m_to_test
            prediction_batch['question']['input_ids'] = i_i_to_test
            prediction_batch_list.append(prediction_batch)
        return prediction_batch_list

    def evaluate(self):
        total_prediction_loss = 0
        hit_1_counter = 0
        hit_2_counter = 0
        total_case_counter = 0
        with torch.no_grad():
            tmp = None
            tmp2 = None
            tmp3 = None
            tmp4 = None
            for positive_set, negative_set in self.test_dataloader:
                total_prediction_loss += self.model(positive_set, negative_set).mean().item()
                # TODO: write the hit-k here
                # TODO: concat the first positive and all the other negative samples without changing the shape
                # self.model.predict(positive_set)

            val_batch_counter = 0

        return total_prediction_loss

        #  print(f'total_prediction_loss: {total_prediction_loss}')

        #     for positive_triplet, _ in tqdm(self.sample_ds):
        #         val_batch_counter += 1
        #         e_h = positive_triplet['e_h']
        #         e_t = positive_triplet['e_t']
        #         question = positive_triplet['question']
        #         e_h_label = self.entity_labels[e_h]
        #         e_t_label = self.entity_labels[e_t]
        #         tmp2 = (e_h_label, e_t_label)
        #
        #         all_possible_e_t_idx = [self.entity_labels.index(e) for e in self.entity_labels
        #                                 if '_' in e and e.startswith(e_h_label + '_')]
        #
        #         ground_truth_idx = all_possible_e_t_idx.index(e_t)
        #         tmp3 = ground_truth_idx
        #         triplet_num = len(all_possible_e_t_idx)
        #         tmp4 = all_possible_e_t_idx
        #
        #         e_h_batch = torch.tensor(e_h).repeat(triplet_num).type(torch.LongTensor)
        #         e_t_batch = torch.tensor(all_possible_e_t_idx).type(torch.LongTensor)
        #
        #         a_m_batch = torch.cat(triplet_num * [question['attention_mask']])
        #         i_i_batch = torch.cat(triplet_num * [question['input_ids']])
        #
        #         q_batch = {'attention_mask': a_m_batch,
        #                    'input_ids': i_i_batch}
        #         test_batch = {'question': q_batch, 'e_h': e_h_batch, 'e_t': e_t_batch}
        #
        # for positive_triplet, _ in tqdm(self.sample_ds):
        #     predictions = self.model.predict(positive_triplet)
        #     tmp = predictions
        #     # _, indices_top_k = torch.topk(k=1, input=predictions, largest=False)
        #
        #     total_case_counter += 1
        #     hit_1_counter += self.hit_at_k(predictions, ground_truth_idx, 1)
        #     hit_2_counter += self.hit_at_k(predictions, ground_truth_idx, 2)

        # print(f'hit_1 {hit_1_counter} out of {total_case_counter}, ratio: {hit_1_counter / total_case_counter}')
        # print(f'hit_2 {hit_2_counter} out of {total_case_counter}, ratio: {hit_2_counter / total_case_counter}')
        # print(f'total_prediction_loss: {total_prediction_loss}')
        # print('example prediction', tmp)
        # print('labels', tmp2)
        # print('ground truth', tmp3)
        # print('all possible', tmp4)
        # print('all possible', [self.entity_labels[t] for t in tmp4])


def grid_search():
    l_r_list = [5e-2, 5e-3, 5e-4, 5e-5, 5e-6]
    n_r_list = [x for x in range(2, 20, 2)]
    dropout = [x / 10 for x in range(1, 9)]
    with open('training_log', 'w') as f:
        f.write('started at:' + str(datetime.datetime.now()))
        f.write('\n')
        f.write(json.dumps(l_r_list))
        f.write('\n')
        f.write(json.dumps(n_r_list))
        f.write('\n')
        f.write(json.dumps(dropout))
        f.write('\n')

    total_comb = len(l_r_list) * len(n_r_list) * len(dropout)
    print(total_comb)
    for l_r in l_r_list:
        for n_r in n_r_list:
            for d_r in dropout:
                # trainer = Trainer()
                # trainer.train()

                START_TIME = time.time()
                meta_data = f' learning rate {l_r}\n' \
                            f' negative rate {n_r}\n' \
                            f' dropout {d_r}\n'

                my_trainer = Trainer(epoches=10, learning_rate=l_r, negative_rate=n_r, drop_out=d_r)
                train_loss, val_loss, init_train_loss, init_val_loss = my_trainer.train()

                results = f' train loss {train_loss}\n' \
                          f' val loss {val_loss}\n' \
                          f' init_train_loss {init_train_loss}\n' \
                          f' init_val_loss {init_val_loss}\n' \
                          f' delta train {init_train_loss - train_loss}\n' \
                          f' delta val {init_val_loss - val_loss}\n'

                content = meta_data + '\n' + results + '\n' + f'time taken {time.time() - START_TIME}\n'
                with open('training_log', 'a') as f:
                    f.write(content)
                    f.write('=========================================\n')


def fine_tuning():
    """
    Only search dropout > 0.5, there is clearly some overfitting
    negative rate set to be max 18
    learning rate to be decided
    :return:
    """

    dropout_list = [0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8]
    negative_rate = [n_r for n_r in range(4, 18, 2)]
    print(negative_rate)


if __name__ == '__main__':
    # grid_search()
    fine_tuning()
