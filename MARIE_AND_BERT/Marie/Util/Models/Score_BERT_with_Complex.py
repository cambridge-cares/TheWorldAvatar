import torch.utils.data
from torch import nn
from transformers import BertModel, BertTokenizer


class Trainer:
    def __init__(self):
        pass


class Dataset(torch.utils.data.Dataset):
    """
    contains: question, head_entity, tail_entity ...

    """

    def __init__(self):
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')

    def __len__(self):
        pass

    def __getitem__(self, item):
        pass


class ScoreBERTWithComplex:
    """
    Use bert to learn a projection (to some extend derive new relations)
    Make sure that the projection leads from head to tail
    """

    def __init__(self):
        self.bert = BertModel.from_pretrained('bert-base-cased')
        # reduce the dimension of the bert output to 2x the ent_dim/ rel_dim
        # split in the vector into re_r and im_r, do the complex scoring
        # rank the candidates and get the loss .

    def score(self, head_idx, proj_rel, tail_idx):
        """
        h, r, t, where real (h,r,t) get the score larger than 1 and r derived by the BERT model
        h removed from the q.
        :param triple:
        :return:
        """
        re_head = self.re_ent(head_idx)
        im_head = self.im_ent(head_idx)

        re_tail = self.re_ent(tail_idx)
        im_tail = self.im_ent(tail_idx)

        pred = - torch.sum(re_head * re_tail * re_rel + im_head * im_tail * re_rel +
                           re_head * im_tail * im_rel - im_head * re_tail * im_rel, -1)

    def forward(self):
        pass
