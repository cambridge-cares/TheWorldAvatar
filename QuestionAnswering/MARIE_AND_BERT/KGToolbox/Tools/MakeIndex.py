from Marie.Util.location import DATA_DIR
import os, pickle
import pandas as pd


def create_indexing(dataset_name='pubchem500', data_dir=None):
    if data_dir is None:
        full_dir = DATA_DIR
    else:
        full_dir = os.path.join(DATA_DIR, data_dir)

    dataset_name = f"{dataset_name}-train.txt"
    df_dataset = pd.read_csv(os.path.join(full_dir, dataset_name), sep='\t', header=None)

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
    file = open(os.path.join(full_dir, 'entity2idx.pkl'), 'wb')
    pickle.dump(entity2idx, file)
    file = open(os.path.join(full_dir, 'relation2idx.pkl'), 'wb')
    pickle.dump(relation2idx, file)
    file = open(os.path.join(full_dir, 'idx2entity.pkl'), 'wb')
    pickle.dump(idx2entity, file)
    file = open(os.path.join(full_dir, 'idx2relation.pkl'), 'wb')
    pickle.dump(idx2relation, file)
    file = os.path.join(full_dir, 'ent_labels.tsv')
    ent_labels = '\n'.join(entities)
    with open(file, 'w') as f:
        f.write(ent_labels)
        f.close()

    print(f'Pickle files are saved to {full_dir}')


if __name__ == '__main__':
    ontology = "fb15k"
    create_indexing("fb15k", data_dir= f'CrossGraph/{ontology}')