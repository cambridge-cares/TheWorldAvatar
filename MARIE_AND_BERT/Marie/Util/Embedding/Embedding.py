import os
from random import sample

import pandas as pd
import torch

from Marie.Util.location import EMBEDDING_DIR

'''
This tool provides embedding search based on IRI of entities ... 
'''


class Embedding():

    def __init__(self):
        self.embedding_path = EMBEDDING_DIR
        self.ent_embedding = self.load_embedding('ent_embedding.tsv')
        self.ent_mapping = self.load_mapping('ent_labels.tsv', self.ent_embedding)

        self.rel_embedding = self.load_embedding('rel_embedding.tsv')
        self.rel_mapping = self.load_mapping('rel_labels.tsv', self.rel_embedding)

        self.ent_list = list(self.ent_mapping.keys())
        self.norm = 1

    def load_embedding(self, target_file):
        # 220 entities with 10 dimensions
        # meanwhile, the question is a vector of 24 ... somehow they need to be ...
        ent_embedding_path = os.path.join(self.embedding_path, target_file)
        ent_embedding = pd.read_csv(ent_embedding_path, sep='\t', header=None)
        return ent_embedding

    def load_mapping(self, target_file, target_embedding):
        mapping_path = os.path.join(self.embedding_path, target_file)
        mapping = pd.read_csv(mapping_path, sep='\t', header=None)
        name_embedding_mapping = {}
        label_list = [_label[0] for _label in mapping.values.tolist()]
        for index, embedding in target_embedding.iterrows():
            embedding_list = embedding.to_list()
            entity_label = label_list[index]
            name_embedding_mapping[entity_label] = embedding_list
        return name_embedding_mapping

    def ent2embedding(self, ent_name):
        if ent_name not in self.ent_mapping:
            return None
        else:
            return self.ent_mapping[ent_name]

    def embedding2ent(self, ent_embed):
        return list(self.ent_mapping.keys())[list(self.ent_mapping.values()).index(ent_embed)][0]

    def rel2embedding(self, rel_name):
        if rel_name not in self.rel_mapping:
            return None
        else:
            return self.rel_mapping[rel_name]

    def name2embedding_batch(self, name_list):
        return pd.DataFrame([self.ent2embedding(name) for name in name_list])

    # TODO: transE scoring .
    def get_transe_score(self, head, rel, tail):
        head = torch.Tensor(head).to('cuda')
        rel = torch.Tensor(rel).to('cuda')
        tail = torch.Tensor(tail).to('cuda')

        return (head + rel - tail).norm(p=self.norm) / 10


if __name__ == '__main__':
    my_embedding = Embedding()
    head = my_embedding.ent2embedding('CID3')
    rel = my_embedding.rel2embedding('canonical_smiles')
    tmp = {}
    # candidates = sample(my_embedding.ent_list, 20) + ['CID2_canonical_smiles']
    candidates = my_embedding.ent_list
    for tail_name in candidates:
        tail = my_embedding.ent2embedding(tail_name)
        score_2 = my_embedding.get_transe_score(head, rel, tail).item()
        tmp[tail_name] = score_2

    ranked = {k: v for k, v in sorted(tmp.items(), key=lambda item: item[1])}
    print(ranked)


