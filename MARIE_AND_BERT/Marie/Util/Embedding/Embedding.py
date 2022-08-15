import os

import pandas as pd
from Marie.Util.location import EMBEDDING_DIR

'''
This tool provides embedding search based on IRI of entities ... 
'''


class Embedding():

    def __init__(self):
        self.embedding_path = EMBEDDING_DIR
        self.ent_embedding = self.load_ent_embedding()
        self.ent_mapping = self.load_ent_mapping()

    def load_ent_embedding(self):
        # 220 entities with 10 dimensions
        # meanwhile, the question is a vector of 24 ... somehow they need to be ...
        ent_embedding_path = os.path.join(self.embedding_path, 'ent_embedding.tsv')
        ent_embedding = pd.read_csv(ent_embedding_path, sep='\t', header=None)
        return ent_embedding

    def load_ent_mapping(self):
        ent_mapping_path = os.path.join(self.embedding_path, 'ent_labels.tsv')
        ent_mapping = pd.read_csv(ent_mapping_path, sep='\t', header=None)

        name_embedding_mapping = {}
        label_list = [ent_label[0] for ent_label in ent_mapping.values.tolist()]
        for index, embedding in self.ent_embedding.iterrows():
            embedding_list = embedding.to_list()
            entity_label = label_list[index]
            name_embedding_mapping[entity_label] = embedding_list
        return name_embedding_mapping

    def name2embedding(self, name):
        if name not in self.ent_mapping:
            print('==================== we have a none ========================',name)

            return None
        else:
            return self.ent_mapping[name]

    def name2embedding_batch(self, name_list):

        return pd.DataFrame([self.name2embedding(name) for name in name_list])


if __name__ == '__main__':
    my_embedding = Embedding()
    rst = my_embedding.name2embedding('CID2_count_heavy_atoms')
    print(rst)
