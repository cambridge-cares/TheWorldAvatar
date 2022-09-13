# ======================= init dependencies and directory info ===================
import os
import pickle
import pandas as pd

from Marie.Util.location import DATA_DIR
from Training.Embedding.TransE_Trainer import Trainer


# STEP 1:
# create entity2idx.pickle and relation2idx.pickle
def create_indexing(dataset_name='pubchem500'):
    dataset_name = f"{dataset_name}-train.txt"
    df_dataset = pd.read_csv(os.path.join(DATA_DIR, dataset_name), sep='\t', header=None)

    # make a set of relations
    relations = sorted(list(set(df_dataset[1].tolist())))
    relation2idx = {}
    for rel_idx, relation in enumerate(relations):
        relation2idx[relation] = rel_idx
    # ================================================================================
    # make a set of entities
    entities = sorted(list(set(df_dataset[0].tolist() + df_dataset[2].tolist())))
    entity2idx = {}
    for ent_idx, entity in enumerate(entities):
        entity2idx[entity] = ent_idx

    # ================================================================================
    # write the dictionaries to pickle
    entity_file = open(os.path.join(DATA_DIR, 'entity2idx.pkl'), 'wb')
    pickle.dump(entity2idx, entity_file)
    relation_file = open(os.path.join(DATA_DIR, 'relation2idx.pkl'), 'wb')
    pickle.dump(relation2idx, relation_file)
    print(f'Pickle files are saved to {DATA_DIR}')


def train_TransE():
    # my_transe_trainer = Trainer()
    pass


if __name__ == '__main__':
    dataset_name = 'pubchem500'
    create_indexing(dataset_name=dataset_name)
