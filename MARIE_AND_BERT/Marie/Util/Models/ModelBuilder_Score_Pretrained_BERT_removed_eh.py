import sys

from torch.utils.data import Subset

sys.path.append('../../../')

import os
import numpy as np
import pandas as pd
import torch.utils.data
from torch import nn, optim
from torch.nn import MarginRankingLoss
from tqdm import tqdm
from Marie.Util.Dataset.ScoringModel_Dataset import Dataset
from Marie.Util.location import TRAINING_DIR, DEPLOYMENT_DIR
from Marie.Util.Models.StandAloneBERT2Embedding import StandAloneBERT
from Marie.Util.Logging import MarieLogger

class ScoreModel(nn.Module):

    def __init__(self, device, model_name, dropout=0.1, for_training=False):
        super(ScoreModel, self).__init__()
        self.logger = MarieLogger()
        self.criterion = MarginRankingLoss(margin=1)  # to make sure that the positive triplet always have smaller
        # distance than negative ones
        self.device = device
        self.bert_with_reduction = StandAloneBERT(device=device)
        self.bert_with_reduction = self.bert_with_reduction.to(self.device)
        self.bert_with_reduction.load_model(model_name)
        if for_training:
            self.data_dir = TRAINING_DIR
        else:
            self.data_dir = DEPLOYMENT_DIR
        self.rel_embedding = pd.read_csv(os.path.join(self.data_dir, 'rel_embedding.tsv'), sep='\t', header=None)
        self.ent_embedding = pd.read_csv(os.path.join(self.data_dir, 'ent_embedding.tsv'), sep='\t', header=None)

    def forward(self, positive_triplets, negative_triplets):
        """
        In middle, measures x1 = dist(positive), x2 = dist(negative), where x1 is minimized, x2 is maximized

        :return: loss function based on the positive and negative component of the triple scoring function
                MarginRankLoss
        """
        # ================= layer 1 ==================
        nlp_components_pos = positive_triplets['question']
        nlp_components_neg = negative_triplets['question']

        projected_rel_pos = self.bert_with_reduction.predict(nlp_components_pos).to(self.device)
        projected_rel_neg = self.bert_with_reduction.predict(nlp_components_neg).to(self.device)

        dist_positive = self.distance(positive_triplets, projected_rel_pos)
        dist_negative = self.distance(negative_triplets, projected_rel_neg)
        return self.loss(dist_positive, dist_negative)

    def loss(self, dist_positive, dist_negative):
        target = torch.tensor([-1], dtype=torch.long).to(self.device)  # set p = -1
        return self.criterion(dist_positive.to(self.device), dist_negative.to(self.device), target).to(self.device)

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


class Trainer:

    def __init__(self, epoches=20, negative_rate=20, learning_rate=5e-4, drop_out=0.1, resume=False, frac=0.1,
                 batch_size=8, model_name='bert_embedding_10000', device=torch.device('cuda')):
        self.df_path = os.path.join(TRAINING_DIR, 'question_set_full')
        self.df = pd.read_csv(self.df_path, sep='\t')
        self.frac = frac
        self.df = self.df.sample(frac=self.frac)
        self.device = device

        self.df_train, self.df_test = np.split(self.df.sample(frac=1, random_state=42), [int(.8 * len(self.df))])

        self.batch_size = batch_size
        self.epoches = epoches
        self.learning_rate = learning_rate
        self.step = 0
        self.test_frequency = 1
        self.drop_out = drop_out

        self.neg_rate = negative_rate
        self.train_set = Dataset(self.df_train, negative_rate=self.neg_rate)
        self.test_set = Dataset(self.df_test, negative_rate=self.neg_rate)
        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=self.batch_size, shuffle=True)

        self.ent_embedding = self.train_set.ent_embedding
        self.model = ScoreModel(model_name=model_name, device=self.device, dropout=self.drop_out)
        self.optimizer = optim.Adam(self.model.parameters(), lr=self.learning_rate)
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
        # TODO: Test with both Adam and SGD
        init_train_loss = 0
        init_val_loss = 0
        total_loss_train = 0
        with tqdm(total=self.epoches, unit=' epoch') as tepoch:
            model = self.model.cuda()
            for epoch_num in range(self.epoches):
                tepoch.set_description(f'Epoch {epoch_num}')
                # model.train()
                # total_loss_train = 0
                # for positive_set, negative_set in tqdm(self.train_dataloader):
                #     self.optimizer.zero_grad()
                #     loss = model(positive_set, negative_set)
                #     # loss.mean().backward()
                #     loss.backward()
                #     loss = loss.data.cuda()
                #     self.optimizer.step()
                #     self.step += 1
                #     total_loss_train += loss.mean().item()
                # print(f'total_loss_train: {total_loss_train}')

                if epoch_num % self.test_frequency == 0:
                    total_loss_val = self.evaluate()

                    torch.save(model.state_dict(), os.path.join(TRAINING_DIR, 'score_model'))

                    print('total_loss_val', total_loss_val)

                    if epoch_num == 0:
                        init_train_loss = total_loss_train
                        init_val_loss = total_loss_val

            # self.model.save('score_model')

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
        hit_5_counter = 0
        total_case_counter = 0
        with torch.no_grad():
            tmp = None
            tmp2 = None
            tmp3 = None
            tmp4 = None
            for positive_set, negative_set in tqdm(self.test_dataloader):
                total_prediction_loss += self.model(positive_set, negative_set).mean().item()
                # TODO: write the hit-k here
                # TODO: concat the first positive and all the other negative samples without changing the shape
                # self.model.predict(positive_set)
                # for prediction_batch in self.make_prediction_batch(positive_set, negative_set):
                #     print(prediction_batch)
                #     predictions = self.model.predict(prediction_batch)
                #     print(self.hit_at_k(predictions, 0, 10))

            val_batch_counter = 0

            for positive_triplet, _ in tqdm(self.sample_ds):
                val_batch_counter += 1
                e_h = positive_triplet['e_h'][0]
                e_t = positive_triplet['e_t'][0]
                # ground_truth_score = self.model.predict(positive_triplet)
                # print(ground_truth_score)

                e_h_label = self.entity_labels[e_h]
                e_t_label = self.entity_labels[e_t]
                all_possible_e_t_idx = [self.entity_labels.index(e) for e in self.entity_labels
                                        if '_' in e and e.startswith(e_h_label + '_') and e != e_t_label]
                all_possible_e_t_idx = [e_t] + all_possible_e_t_idx
                ground_truth_idx = 0
                triplet_num = len(all_possible_e_t_idx)
                e_h_batch = torch.tensor(e_h).repeat(triplet_num).type(torch.LongTensor)
                e_t_batch = torch.tensor(all_possible_e_t_idx).type(torch.LongTensor)

                question = positive_triplet['question']
                a_m_batch = torch.tensor(question['attention_mask'][0]).type(torch.LongTensor).repeat(triplet_num).type(
                    torch.LongTensor)
                i_i_batch = torch.tensor(question['input_ids'][0]).type(torch.LongTensor).repeat(triplet_num).type(
                    torch.LongTensor)

                q_batch = {'attention_mask': a_m_batch,
                           'input_ids': i_i_batch}
                test_batch = {'question': q_batch, 'e_h': e_h_batch, 'e_t': e_t_batch}

                predictions = self.model.predict(test_batch)
                # _, indices_top_k = torch.topk(k=1, input=predictions, largest=False)
                tmp = predictions
                hit_1_counter += self.hit_at_k(predictions, ground_truth_idx, 1)
                hit_2_counter += self.hit_at_k(predictions, ground_truth_idx, 2)
                hit_5_counter += self.hit_at_k(predictions, ground_truth_idx, 5)
                total_case_counter += 1

        print('hit_1_counter', hit_1_counter)
        print('hit_2_counter', hit_2_counter)
        print('hit_5_counter', hit_5_counter)
        print('out of', total_case_counter)
        print('example prediction', pd.DataFrame(tmp.cpu()))
        print('ground truth', tmp3)
        return total_prediction_loss


def build_from_optimal_parameters():
    dropout = 0
    neg_rate = 1
    learning_rate = 1e-24
    epochs = 300
    frac = 1
    my_trainer = Trainer(epoches=epochs, negative_rate=neg_rate, learning_rate=learning_rate, drop_out=dropout,
                         frac=frac)
    train_loss, val_loss, init_train_loss, init_val_loss = my_trainer.train()


if __name__ == '__main__':
    build_from_optimal_parameters()
