# ======================= init dependencies and directory info ===================
import os
import pickle
import pandas as pd

from Marie.Util.location import DATA_DIR
from Training.Embedding.TransE_Trainer import Trainer

# STEP 0
# create training set of size x

# STEP 1:
# create entity2idx.pickle and relation2idx.pickle
def create_indexing(dataset_name='pubchem500', data_dir = None, DATA_DIR = DATA_DIR):

    if data_dir is not None:
        DATA_DIR = os.path.join(DATA_DIR, data_dir)

    dataset_name = f"{dataset_name}-train.txt"
    df_dataset = pd.read_csv(os.path.join(DATA_DIR, dataset_name), sep='\t', header=None)

    # make a set of relations
    relations = sorted(list(set(df_dataset[1].tolist())))
    relation2idx = {}
    idx2relation = {}
    for rel_idx, relation in enumerate(relations):
        relation2idx[relation] = rel_idx
        idx2relation[rel_idx] = relation
    # ================================================================================
    # make a set of entities
    entities = sorted(list(set(df_dataset[0].tolist() + df_dataset[2].tolist())))
    entity2idx = {}
    idx2entity = {}
    for ent_idx, entity in enumerate(entities):
        entity2idx[entity] = ent_idx
        idx2entity[ent_idx] = entity

    # ================================================================================
    # write the dictionaries to pickle
    file = open(os.path.join(DATA_DIR, 'entity2idx.pkl'), 'wb')
    pickle.dump(entity2idx, file)
    file = open(os.path.join(DATA_DIR, 'relation2idx.pkl'), 'wb')
    pickle.dump(relation2idx, file)
    file = open(os.path.join(DATA_DIR, 'idx2entity.pkl'), 'wb')
    pickle.dump(idx2entity, file)
    file = open(os.path.join(DATA_DIR, 'idx2relation.pkl'), 'wb')
    pickle.dump(idx2relation, file)
    file = os.path.join(DATA_DIR,'ent_labels.tsv')
    ent_labels = '\n'.join(entities)
    with open(file, 'w') as f:
        f.write(ent_labels)
        f.close()

    print(f'Pickle files are saved to {DATA_DIR}')


def train_TransE(dataset_name):
    my_transe_trainer = Trainer(dataset_name=dataset_name, load_pretrained_embeddings=True)
    my_transe_trainer.train()


if __name__ == '__main__':
    dataset_name = 'pubchem50000'
   # create_indexing(dataset_name=dataset_name, data_dir= 'pubchem_50000')
    train_TransE(dataset_name=dataset_name)