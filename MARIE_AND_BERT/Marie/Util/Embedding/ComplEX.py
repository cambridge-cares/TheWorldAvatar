import os
import random

import torch
from torch import nn
from torch.nn import Embedding
from torch.nn.init import xavier_uniform_
from tqdm import tqdm

from Marie.Util.Models.TransE_Dataset import Dataset
from Marie.Util.location import DATASET_DIR, DATA_DIR, EMBEDDING_DIR


#         self.model = TransE(dim=self.dim, ent_num=self.e_num, rel_num=self.r_num, resume_training=False, device=device)
class ComplEX(nn.Module):

    def __init__(self, ent_num, rel_num, emb_dim):
        super(ComplEX, self).__init__()

        self.rel_num = rel_num
        self.ent_num = ent_num
        self.emb_dim = emb_dim
        self.re_ent_emb = self.init_embedding(self.ent_num, self.emb_dim)
        self.im_ent_emb = self.init_embedding(self.ent_num, self.emb_dim)
        self.re_rel_emb = self.init_embedding(self.rel_num, self.emb_dim)
        self.im_rel_emb = self.init_embedding(self.rel_num, self.emb_dim)

        self.criterion = nn.MarginRankingLoss(margin=0)

    def init_embedding(self, num, dim):
        """Create a torch.nn.Embedding object with `n_vectors` samples and `dim`
        dimensions. It is then initialized with Xavier uniform distribution.
        """
        entity_embeddings = Embedding(num, dim)
        xavier_uniform_(entity_embeddings.weight.data)
        return entity_embeddings

    def predict(self, triplets):
        return self.distance(triplets)

    def distance(self, triplet):
        h_idx = triplet[0]
        r_idx = triplet[1]
        t_idx = triplet[2]
        re_h, im_h = self.re_ent_emb(h_idx), self.im_ent_emb(h_idx)
        re_t, im_t = self.re_ent_emb(t_idx), self.im_ent_emb(t_idx)
        re_r, im_r = self.re_rel_emb(r_idx), self.im_rel_emb(r_idx)

        return (re_h * (re_r * re_t + im_r * im_t) + im_h * (
                re_r * im_t - im_r * re_t)).sum(dim=1)

    def loss(self, positive_distances, negative_distances):
        target = torch.tensor([-1], dtype=torch.long)
        return self.criterion(positive_distances, negative_distances, target)

    def forward(self, positive_triplets, negative_triplets):
        pos_distance = self.distance(positive_triplets)
        neg_distance = self.distance(negative_triplets)
        return self.loss(pos_distance, neg_distance)


class Trainer:

    def __init__(self):
        self.epochs = 100
        self.dataset_name = 'pubchem5000'
        self.batch_size = 64
        self.learning_rate = 1
        self.step = 0

        train_triplets = random.sample([line.split('\t') for line in
                                        open(os.path.join(DATA_DIR,
                                                          f'{self.dataset_name}-train.txt')).read().splitlines()], 1000)

        test_triplets = random.sample([line.split('\t') for line in
                                       open(os.path.join(DATA_DIR,
                                                         f'{self.dataset_name}-test.txt')).read().splitlines()], 200)

        self.train_set = Dataset(train_triplets)
        self.test_set = Dataset(test_triplets)
        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=self.batch_size, shuffle=True)

        self.e_num = self.train_set.ent_num
        self.r_num = self.train_set.rel_num
        self.model = ComplEX(ent_num=self.e_num, rel_num=self.r_num, emb_dim=50)

        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)

    def train(self):
        for epoch_num in tqdm(range(self.epochs)):
            self.model.train()
            total_loss_train = 0
            for pos_train, neg_train in tqdm(self.train_dataloader):
                self.optimizer.zero_grad()
                loss = self.model(pos_train, neg_train)
                loss.mean().backward()
                loss = loss.data.cuda()
                self.optimizer.step()
                self.step += 1
                total_loss_train += loss.mean().item()
                if epoch_num % 20 == 0:
                    self.evaluate()
            print('total_loss_train: ', total_loss_train)

    def hit_at_k(self, predictions, ground_truth_idx, k: int = 10):
        _, indices_top_k = torch.topk(predictions, k=k, largest=False)
        if ground_truth_idx in indices_top_k:
            return 1
        else:
            return 0

    def evaluate(self):
        total_loss_val = 0
        hit_10 = 0
        hit_5 = 0
        hit_1 = 0
        total_case = 0
        self.model.eval()
        with torch.no_grad():
            for positive_triplets, _ in self.test_dataloader:
                prediction = self.model.predict(positive_triplets).mean()
                total_loss_val += prediction

                ground_truth_triplets = torch.transpose(torch.stack(positive_triplets), 0, 1).type(torch.LongTensor)
                for i, triplet in enumerate(ground_truth_triplets):
                    head = triplet[0]
                    rel = triplet[1]
                    tail_true = triplet[2]
                    head_tensor = head.repeat(self.e_num)
                    rel_tensor = rel.repeat(self.e_num)
                    tail_all = torch.range(0, self.e_num - 1).type(torch.LongTensor)
                    new_triplets = torch.stack((head_tensor, rel_tensor, tail_all)).type(torch.LongTensor)
                    prediction = self.model.predict(new_triplets)

                    total_case += 1
                    hit_10 += self.hit_at_k(prediction, tail_true, k=10)
                    hit_5 += self.hit_at_k(prediction, tail_true, k=5)
                    hit_1 += self.hit_at_k(prediction, tail_true, k=1)

        print('Current Hit 10 rate:', hit_10, ' out of ', total_case, ' ratio is: ', hit_10 / total_case)
        print('Current Hit 5 rate:', hit_5, ' out of ', total_case, ' ratio is: ', hit_5 / total_case)
        print('Current Hit 1 rate:', hit_1, ' out of ', total_case, ' ratio is: ', hit_1 / total_case)
        print(f'total_loss_val {total_loss_val}')


if __name__ == '__main__':
    my_trainer = Trainer()
    my_trainer.train()
