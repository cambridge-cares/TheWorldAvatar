import os

import torch
from torch import nn
from torch.nn.functional import normalize
from torch.nn.init import xavier_uniform
from torch import matmul
from Marie.Util.Embedding.EmbeddingTrainer import Trainer
from Marie.Util.Dataset.TransE_Dataset import Dataset
from Marie.Util.location import DATA_DIR


class TransR(nn.Module):

    def __init__(self, rel_dim, rel_num, ent_dim, ent_num):
        super(TransR, self).__init__()
        self.rel_num = rel_num
        self.ent_num = ent_num
        self.rel_dim = rel_dim
        self.ent_dim = ent_dim
        self.ent_embedding = self._init_embedding(num=self.ent_num, dim=self.ent_dim)
        self.rel_embedding = self._init_embedding(num=self.rel_num, dim=self.rel_dim)
        self.proj_matrix = self._init_embedding(num=self.rel_num, dim=self.rel_dim * self.ent_dim)
        self.criterion = nn.MarginRankingLoss(margin=6)
        self.normalize_parameters()

    def loss(self, pos_distance, neg_distance):
        target = torch.tensor([-1], dtype=torch.long)
        return self.criterion(pos_distance, neg_distance, target)

    def _init_embedding(self, num, dim):
        # all apply xavier uniform
        embeddings = nn.Embedding(num_embeddings=num, embedding_dim=dim)
        xavier_uniform(embeddings.weight.data)
        return embeddings

    def forward(self, pos_triples, neg_triples):
        dist_pos = self.distance(pos_triples)
        dist_neg = self.distance(neg_triples)

        return self.loss(dist_pos, dist_neg)

    def predict(self, triples):
        ent_num = self.ent_num
        head = triples[0]
        rel = triples[1]
        true_tail = triples[2]
        # repeat the head and rel
        heads = head.repeat(ent_num)
        rels = rel.repeat(ent_num)
        tails = torch.LongTensor(torch.arange(0, ent_num))
        test_triples = (heads, rels, tails)
        return self.distance(test_triples), true_tail

    def project(self, ent, proj_mat):
        proj_e = matmul(proj_mat, ent.view(-1, self.ent_dim, 1))
        return proj_e.view(-1, self.rel_dim)

    def normalize_parameters(self):
        """Normalize the entity and relation embeddings, as explained in
        original paper. This methods should be called at the end of each
        training epoch and at the end of training as well.
        """
        self.ent_embedding.weight.data = normalize(self.ent_embedding.weight.data,
                                                   p=2, dim=1)
        self.rel_embedding.weight.data = normalize(self.ent_embedding.weight.data,
                                                   p=2, dim=1)

    # def infer_reaction(self, reactants, rel2idx):
    #     r_idx = rel2idx["isReactant"]
    #     r_idx = torch.LongTensor([r_idx])
    #     r = self.rel_embedding(r_idx)
    #     reactants_idx = torch.LongTensor(reactants)
    #     pass




    def distance(self, triples):
        """
        measures triplet score with
        ||p_r(h) + r - p_r(t)||_2^2
        :param triples:
        :return:
        """
        e_h_idx = triples[0]
        r_idx = triples[1]
        e_t_idx = triples[2]

        # =============== prepare embeddings =======================
        e_h = normalize(self.ent_embedding(e_h_idx))
        e_t = normalize(self.ent_embedding(e_t_idx))
        r = self.rel_embedding(r_idx)
        # ==========================================================

        b_size = len(e_h_idx)
        # reshape the proj_mat according to the batch_size
        proj_mat = self.proj_matrix(r_idx).view(b_size, self.rel_dim, self.ent_dim)

        # project e_h and e_t with proj_mat
        projected_e_h = self.project(ent=e_h, proj_mat=proj_mat)
        projected_e_t = self.project(ent=e_t, proj_mat=proj_mat)

        # use l2 dissimilarity
        return (projected_e_h + r - projected_e_t).norm(p=2, dim=-1)


if __name__ == '__main__':
    train_triplets = [line.split('\t') for line in
                      open(os.path.join(DATA_DIR, 'pubchem100-train.txt')).read().splitlines()]

    test_triplets = [line.split('\t') for line in
                     open(os.path.join(DATA_DIR, 'pubchem100-test.txt')).read().splitlines()]

    train_set = Dataset(train_triplets)
    test_set = Dataset(test_triplets)
    rel_dim = 50
    ent_dim = 50
    rel_num = train_set.rel_num
    ent_num = train_set.ent_num
    model = TransR(rel_dim, rel_num, ent_dim, ent_num)
    my_trainer = Trainer(model=model, dataset_name='pubchem100', epochs=5000, learning_rate=0.00001)
    my_trainer.train()
