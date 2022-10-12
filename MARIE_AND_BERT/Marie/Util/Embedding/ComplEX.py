import torch
from torch import nn
from torch.nn import Embedding
from torch.nn.init import xavier_uniform_

from Marie.Util.Embedding.EmbeddingTrainer import Trainer


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


if __name__ == '__main__':
    my_trainer = Trainer(ComplEX)
    my_trainer.train()
