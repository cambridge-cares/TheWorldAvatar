import pickle
import sys

import pandas
import os
import torch
from torch import nn
from torch.nn.init import xavier_uniform_

sys.path.append("../../..")

from Marie.Util.Embedding.EmbeddingTrainer import Trainer
from Marie.Util.location import DATA_DIR


class Complex(nn.Module):

    def __init__(self, dim, ent_num, rel_num, resume_training=True, device='cuda',
                 dataset_dir=None, enable_numerical=False):
        super(Complex, self).__init__()
        self.dim = dim
        self.ent_num = ent_num
        self.rel_num = rel_num
        self.device = device
        self.dataset_dir = dataset_dir
        self.enable_numerical = enable_numerical

        if resume_training:
            print(f"resuming training from {self.dataset_dir}")
            self.re_ent, self.im_ent = self.load_embedding(embedding_name="ent_embedding.tsv")
            self.re_rel, self.im_rel = self.load_embedding(embedding_name="rel_embedding.tsv")
            if self.enable_numerical:
                self.attr = self.load_regular_embedding(embedding_name="attr_embedding.tsv")
                self.bias = self.load_regular_embedding(embedding_name="bias_embedding.tsv")

        else:
            self.re_rel = self._init_rel_embedding()
            self.im_rel = self._init_rel_embedding()
            self.re_ent = self._init_ent_embedding()
            self.im_ent = self._init_ent_embedding()
            if self.enable_numerical:
                self.attr = self._init_attr_embedding()
                self.bias = self._init_bias_embedding()

    def _init_attr_embedding(self):
        """
        :param dim: dimension of the embedding
        :param num: number of entities
        :return:
        """
        attr_embeddings = nn.Embedding(num_embeddings=self.rel_num + 1, embedding_dim=self.dim * 2)
        xavier_uniform_(attr_embeddings.weight.data)
        return attr_embeddings

    def _init_bias_embedding(self):
        """
        :param dim: dimension of the embedding
        :param num: number of entities
        :return:
        """
        bias_embeddings = nn.Embedding(num_embeddings=self.rel_num + 1, embedding_dim=1)
        xavier_uniform_(bias_embeddings.weight.data)
        return bias_embeddings

    def _init_ent_embedding(self):
        """
        :param dim: dimension of the embedding
        :param num: number of entities
        :return:
        """
        entity_embeddings = nn.Embedding(num_embeddings=self.ent_num + 1, embedding_dim=self.dim)
        xavier_uniform_(entity_embeddings.weight.data)
        return entity_embeddings

    def _init_rel_embedding(self):
        rel_embeddings = nn.Embedding(num_embeddings=self.rel_num + 1, embedding_dim=self.dim)
        xavier_uniform_(rel_embeddings.weight.data)
        return rel_embeddings

    def load_regular_embedding(self, embedding_name):
        print(f"loading embedding from {os.path.join(DATA_DIR, self.dataset_dir, f'{embedding_name}')}")
        tsv_file = pandas.read_csv(os.path.join(DATA_DIR, self.dataset_dir, f'{embedding_name}'), sep='\t',
                                   header=None)
        pretrained_regular_embedding = torch.FloatTensor(tsv_file.values)
        return nn.Embedding.from_pretrained(pretrained_regular_embedding).requires_grad_(True)

    def load_embedding(self, embedding_name):
        tsv_file = pandas.read_csv(os.path.join(self.dataset_dir, embedding_name), sep='\t', header=None)
        re, im = tsv_file.iloc[:, : self.dim], tsv_file.iloc[:, self.dim:]
        re = torch.FloatTensor(re.values)
        im = torch.FloatTensor(im.values)
        print(f"done resuming training from {self.dataset_dir}")
        return nn.Embedding.from_pretrained(re).requires_grad_(True), \
               nn.Embedding.from_pretrained(im).requires_grad_(True)

    def forward(self, triples, mode="non_numerical"):
        triples = torch.transpose(triples, 0, 1)
        if mode == "non_numerical":
            # (s, p, o, 1, v)
            target = triples[3]
            pred = self.score(triples).to(self.device)
            return self.pointwise_bce(pred.type(torch.FloatTensor), target.type(torch.FloatTensor))
        else:
            return self.numerical_predict(triples)

    def predict(self, triple):
        return self.score(triple)

    def infer(self, triple):
        return self.score(triple)

    def pointwise_bce(self, preds, target):
        # split the
        loss = torch.nn.BCEWithLogitsLoss()(torch.clamp(preds, min=0.0, max=1.0),
                                            target)  # torch.clamp(target, min=0.0, max=1.0))
        # loss = torch.nn.BCEWithLogitsLoss()(preds, target)
        # loss = F.softplus(target*preds).mean()
        # loss = torch.nn.BCEWithLogitsLoss()(preds, torch.clamp(target, min=0.0, max=1.0))
        return loss

    def numerical_predict(self, triple):
        """
        Alg 1: concat(re_head, im_head) * attr + bias
        :param triple:
        :return:
        """
        # (s, p, o, 1, v)
        head_idx = triple[0].long().to(self.device)
        rel_idx = triple[1].long().to(self.device)
        true_value = triple[4]
        re_head = self.re_ent(head_idx)
        im_head = self.im_ent(head_idx)
        attr = self.attr(rel_idx)
        bias = self.bias(rel_idx)
        head = torch.cat((re_head, im_head), 1)
        aV = torch.sum(head * attr, dim=1).to(self.device)
        value = (aV + bias[0]).to(self.device)
        diff = torch.abs(value - true_value)
        return diff

    def score(self, triple):
        head_idx = triple[0].long().to(self.device)
        rel_idx = triple[1].long().to(self.device)
        tail_idx = triple[2].long().to(self.device)

        re_head = self.re_ent(head_idx)
        im_head = self.im_ent(head_idx)

        re_rel = self.re_rel(rel_idx)
        im_rel = self.re_rel(rel_idx)

        re_tail = self.re_ent(tail_idx)
        im_tail = self.im_ent(tail_idx)
        pred = - torch.sum(re_head * re_tail * re_rel + im_head * im_tail * re_rel +
                           re_head * im_tail * im_rel - im_head * re_tail * im_rel, -1)
        return pred


if __name__ == '__main__':
    resume_training = False
    target_ontology = "ontokin_reactions"
    learning_rate = 1
    neg_rate = 1000
    dim = 80
    batch_size = 512
    print(f"ontology: {target_ontology}")
    print(f"learning_rate: {learning_rate}")
    print(f"neg_rate: {neg_rate}")
    print(f"dim: {dim}")
    print(f"batch_size: {batch_size}")
    full_dir = os.path.join(DATA_DIR, 'CrossGraph', target_ontology)
    print(f"complex embedding for {full_dir}")
    r2i_path = open(os.path.join(full_dir, f'relation2idx.pkl'), 'rb')
    e2i_path = open(os.path.join(full_dir, f'entity2idx.pkl'), 'rb')
    rel_num = len(pickle.load(r2i_path).keys())
    ent_num = len(pickle.load(e2i_path).keys())
    print("initing model")
    model = Complex(dim=dim, rel_num=rel_num, ent_num=ent_num,
                    dataset_dir=os.path.join(DATA_DIR, 'CrossGraph', target_ontology), resume_training=resume_training)
    print("done initing model")
    trainer = Trainer(model=model, dataset_name=target_ontology, epochs=10, learning_rate=learning_rate, test_step=10,
                      pointwise=True, batch_size=batch_size, save_model=True, complex=True, gamma=0.1,
                      data_folder=f"CrossGraph/{target_ontology}", neg_rate=neg_rate, scheduler_step=250)

    trainer.train()
    # print(f"ontology: {target_ontology}")
    # print(f"learning_rate: {learning_rate}")
    # print(f"neg_rate: {neg_rate}")
    # for i in range(0, 10):
    # resume_training = True
    # print(f"ontology: {target_ontology}")
    # print(f"learning_rate: {learning_rate}")
    # print(f"neg_rate: {neg_rate}")
    # print(f"dim: {dim}")
    # print(f"batch_size: {batch_size}")
    # full_dir = os.path.join(DATA_DIR, 'CrossGraph', target_ontology)
    # print(f"complex embedding for {full_dir}")
    # r2i_path = open(os.path.join(full_dir, f'relation2idx.pkl'), 'rb')
    # e2i_path = open(os.path.join(full_dir, f'entity2idx.pkl'), 'rb')
    # rel_num = len(pickle.load(r2i_path).keys())
    # ent_num = len(pickle.load(e2i_path).keys())
    # print("initing model")
    # model = Complex(dim=dim, rel_num=rel_num, ent_num=ent_num,
    # dataset_dir=os.path.join(DATA_DIR, 'CrossGraph', target_ontology), resume_training=resume_training)
    # print("done initing model")
    # trainer = Trainer(model=model, dataset_name=target_ontology, epochs=10, learning_rate=learning_rate, test_step = 100,
    # pointwise=True, batch_size=batch_size, save_model=True, complex=True, gamma=0.1,
    # data_folder=f"CrossGraph/{target_ontology}", neg_rate=neg_rate, scheduler_step = 250)

    # trainer.train()
    # print(f"ontology: {target_ontology}")
    # print(f"learning_rate: {learning_rate}")
    # print(f"neg_rate: {neg_rate}")

    # trainer = Trainer(model=model, dataset_name=target_ontology, epochs=10, learning_rate=learning_rate, test_step = 10,
    # pointwise=True, batch_size=batch_size, save_model=True, complex=True, gamma=0.1,
    # data_folder=f"CrossGraph/{target_ontology}", neg_rate=neg_rate, scheduler_step = 250)
    # trainer.train()

    # for i in range(0, 20):
    # full_dir = os.path.join(DATA_DIR, 'CrossGraph', target_ontology)
    # print(f"complex embedding for {full_dir}")
    # r2i_path = open(os.path.join(full_dir, f'relation2idx.pkl'), 'rb')
    # e2i_path = open(os.path.join(full_dir, f'entity2idx.pkl'), 'rb')
    # rel_num = len(pickle.load(r2i_path).keys())
    # ent_num = len(pickle.load(e2i_path).keys())
    # print("initing model")
    # model = Complex(dim=40, rel_num=rel_num, ent_num=ent_num,
    # dataset_dir=os.path.join(DATA_DIR, 'CrossGraph', target_ontology), resume_training=resume_training)
    # print("done initing model")
    # trainer = Trainer(model=model, dataset_name=target_ontology, epochs=199, learning_rate=1,
    # pointwise=True, batch_size=32, save_model=True, complex=True, gamma=1,
    # data_folder=f"CrossGraph/{target_ontology}")

    # trainer.train()
    # resume_training = True

    # full_dir = os.path.join(DATA_DIR, 'CrossGraph', target_ontology)
    # print(f"complex embedding for {full_dir}")
    # r2i_path = open(os.path.join(full_dir, f'relation2idx.pkl'), 'rb')
    # e2i_path = open(os.path.join(full_dir, f'entity2idx.pkl'), 'rb')
    # rel_num = len(pickle.load(r2i_path).keys())
    # ent_num = len(pickle.load(e2i_path).keys())
    # print("initing model")
    # model = Complex(dim=40, rel_num=rel_num, ent_num=ent_num,
    # dataset_dir=os.path.join(DATA_DIR, 'CrossGraph', target_ontology), resume_training=True)
    # print("done initing model")
    # trainer = Trainer(model=model, dataset_name=target_ontology, epochs=201, learning_rate=1,
    # pointwise=True, batch_size=32, save_model=True, complex=True, gamma=1,
    # data_folder=f"CrossGraph/{target_ontology}")

    # trainer.train()

    # target_ontology = "pubchem500c"
    # full_dir = os.path.join(DATA_DIR, 'Evaluation', target_ontology)
    # print(f"complex embedding for {full_dir}")
    # r2i_path = open(os.path.join(full_dir, f'relation2idx.pkl'), 'rb')
    # e2i_path = open(os.path.join(full_dir, f'entity2idx.pkl'), 'rb')
    # rel_num = len(pickle.load(r2i_path).keys())
    # ent_num = len(pickle.load(e2i_path).keys())
    # print("initing model")
    # model = Complex(dim=40, rel_num=rel_num, ent_num=ent_num,
    # dataset_dir=os.path.join(DATA_DIR, 'Evaluation', target_ontology), resume_training=False)
    # print("with dimension of 40")
    # print("done initing model")
    # trainer = Trainer(model=model, dataset_name=target_ontology, epochs=500, learning_rate=1,
    # pointwise=True, batch_size=32, save_model=False, complex=True, gamma=1,
    # data_folder=f"Evaluation/{target_ontology}")
    # trainer.train()
