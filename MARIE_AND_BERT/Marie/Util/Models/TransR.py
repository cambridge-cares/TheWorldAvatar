import os

import torch
from torch import nn
from torch.nn.functional import normalize
from torch.nn.init import xavier_uniform, xavier_uniform_
from torch import matmul
from torchmetrics import MeanSquaredLogError

from Marie.Util.Embedding.EmbeddingTrainer import Trainer
from Marie.Util.Dataset.TransE_Dataset import Dataset
from Marie.Util.location import DATA_DIR


class TransR(nn.Module):
    """
    TransR is the best translate model that can handle N-to-N relations
    Also aim to implement numerical embedding in the embedding
    """

    def __init__(self, rel_dim, rel_num, ent_dim, ent_num, device="cpu", use_projection=False, alpha=0.1):
        super(TransR, self).__init__()
        self.device = device
        self.rel_num = rel_num
        self.ent_num = ent_num
        self.rel_dim = rel_dim
        self.ent_dim = ent_dim
        self.ent_embedding = self._init_embedding(num=self.ent_num, dim=self.ent_dim).to(self.device)
        self.rel_embedding = self._init_embedding(num=self.rel_num, dim=self.rel_dim).to(self.device)
        self.attr_embedding = nn.Embedding(num_embeddings=self.rel_num, embedding_dim=self.rel_dim)
        self.attr_embedding.weight.data.uniform_(-1, 1)
        self.bias_embedding = self._init_embedding(num=self.rel_num, dim=1).to(self.device)
        self.proj_matrix = self._init_embedding(num=self.rel_num, dim=self.rel_dim * self.ent_dim).to(self.device)

        self.criterion = nn.MarginRankingLoss(margin=6).to(self.device)
        self.normalize_parameters()
        self.msle_loss = MeanSquaredLogError()
        self.use_projection = use_projection
        self.alpha = alpha

    def loss(self, pos_distance, neg_distance):
        target = torch.tensor([-1], dtype=torch.long).to(self.device)
        return self.criterion(pos_distance, neg_distance, target).to(self.device)

    def _init_ent_embedding(self):
        ent_embedding = nn.Embedding(embedding_dim=self.ent_dim, num_embeddings=self.ent_num + 1,
                                     padding_idx=self.ent_num)
        ent_embedding.weight.data.uniform_(-1, 1)
        # xavier_uniform_(ent_embedding.weight.data)
        return ent_embedding

    def _init_attr_embedding(self):
        attr_embedding = nn.Embedding(embedding_dim=self.rel_dim, num_embeddings=self.rel_num + 1,
                                      padding_idx=self.rel_num)
        # attr_embedding.weight.data.uniform_(-1, 1)
        # xavier_uniform_(attr_embedding.weight.data)

        return attr_embedding

    def _init_rel_embedding(self):
        rel_embedding = nn.Embedding(embedding_dim=self.rel_dim, num_embeddings=self.rel_num + 1,
                                     padding_idx=self.rel_num)
        rel_embedding.weight.data.uniform_(-1, 1)
        #  rel_embedding.weight.data[:-1, :].div_(rel_embedding.weight.data[:-1, :].norm(p=1, dim=1, keepdim=True))
        return rel_embedding

    def _init_embedding(self, num, dim):
        # all apply xavier uniform
        embeddings = nn.Embedding(num_embeddings=num, embedding_dim=dim)
        xavier_uniform_(embeddings.weight.data)
        return embeddings

    def forward(self, pos_triples, neg_triples, mode="non_numerical"):
        dist_pos = self.distance(pos_triples)
        dist_neg = self.distance(neg_triples)
        if mode == "non_numerical":
            return self.loss(dist_pos, dist_neg).mean()
        else:
            numerical_loss = self.numerical_forward(pos_triples)  # numerical loss only requires pos triples
            return (1 - self.alpha) * self.loss(dist_pos, dist_neg).mean() + self.alpha * numerical_loss

    def numerical_forward(self, triples):
        true_value = triples[3].to(self.device)
        head_idx = triples[0].long().to(self.device)
        rel_idx = triples[1].long().to(self.device)
        if len(rel_idx) == 0:
            return 0
        head = self.ent_embedding(rel_idx).to(self.device)
        if self.use_projection:
            b_size = len(head_idx)
            proj_mat = self.proj_matrix(rel_idx).view(b_size, self.rel_dim, self.ent_dim)
            head = self.project(ent=head, proj_mat=proj_mat).to(self.device)
        attr = self.attr_embedding(rel_idx.to(self.device)).to(self.device)
        bias = self.bias_embedding(rel_idx.to(self.device)).to(self.device)
        aV = torch.sum(head * attr, dim=1).to(self.device)
        value = (aV + bias)[0].mean().to(self.device)
        diff = self.msle_loss(value, true_value.mean()).mean().to(self.device)
        return diff.to(self.device)

    def predict(self, triples):
        head = triples[0]
        rel = triples[1]
        tail = triples[2]
        test_triples = (head, rel, tail)
        return self.distance(test_triples)

    def infer(self, triples):
        heads = triples[0]
        rels = triples[1]
        tails = triples[2]

        test_triples = (heads, rels, tails)
        return self.distance(test_triples)  # , true_tail, candidates_tails

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
        self.rel_embedding.weight.data = normalize(self.rel_embedding.weight.data,
                                                   p=2, dim=1)

        # self.attr_embedding.weight.data = normalize(self.attr_embedding.weight.data,
        #                                             p=2, dim=1)
        # self.bias_embedding.weight.data = normalize(self.bias_embedding.weight.data,
        #                                             p=2, dim=1)

    # def infer_reaction(self, reactants, rel2idx):
    #     r_idx = rel2idx["isReactant"]
    #     r_idx = torch.LongTensor([r_idx])
    #     r = self.rel_embedding(r_idx)
    #     reactants_idx = torch.LongTensor(reactants)
    #     pass

    def regularization(self, triples):
        e_h_idx = triples[0]
        r_idx = triples[1]
        e_t_idx = triples[2]
        value = triples[3]

        # =============== prepare embeddings =======================
        e_h = normalize(self.ent_embedding(e_h_idx))
        e_t = normalize(self.ent_embedding(e_t_idx))
        r = self.rel_embedding(r_idx)
        b_size = len(e_h_idx)
        proj_mat = self.proj_matrix(r_idx).view(b_size, self.rel_dim, self.ent_dim)
        regul = (torch.mean(e_h ** 2) +
                 torch.mean(e_t ** 2) +
                 torch.mean(r ** 2) +
                 torch.mean(proj_mat ** 2)) / 4
        return regul

    def distance(self, triples):
        """
        measures triplet score with
        ||p_r(h) + r - p_r(t)||_2^2
        :param triples:
        :return:
        """
        e_h_idx = torch.LongTensor(triples[0].long()).to(self.device)
        r_idx = torch.LongTensor(triples[1].long()).to(self.device)
        e_t_idx = torch.LongTensor(triples[2].long()).to(self.device)

        # =============== prepare embeddings =======================
        e_h = normalize(self.ent_embedding(e_h_idx)).to(self.device)
        e_t = normalize(self.ent_embedding(e_t_idx)).to(self.device)
        r = self.rel_embedding(r_idx).to(self.device)
        # ==========================================================

        b_size = len(e_h_idx)
        # reshape the proj_mat according to the batch_size
        proj_mat = self.proj_matrix(r_idx).view(b_size, self.rel_dim, self.ent_dim).to(self.device)

        # project e_h and e_t with proj_mat
        projected_e_h = self.project(ent=e_h, proj_mat=proj_mat).to(self.device)
        projected_e_t = self.project(ent=e_t, proj_mat=proj_mat).to(self.device)

        # use l2 dissimilarity
        return (projected_e_h + r - projected_e_t).norm(p=2, dim=-1).to(self.device)


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
