import json
import os
import pickle
import time
import sys
import pandas as pd

sys.path.append('../..')
from Marie.Util.location import DATA_DIR


class HopExtractor:
    """
    The Hop Extractor class extracts n-hop neighbours of a certain entity.
    """

    def __init__(self, dataset_dir, dataset_name, n: int = 3, numerical=False):
        """
        Initialize the extractor instance
        :param dataset_dir: the folder of the triples and indexing files
        :param dataset_name: the names of the dataset
        :param n: the maximum distance between the head entity and the neighbours
        """
        self.numerical = numerical
        self.dataset_dir = dataset_dir
        self.dataset_name = dataset_name
        self.n = n
        self.triples_full_path = os.path.join(DATA_DIR, self.dataset_dir, f'{self.dataset_name}-train.txt')
        self.triples = pd.read_csv(self.triples_full_path, sep='\t', header=None)
        self.entity2idx_path = os.path.join(DATA_DIR, self.dataset_dir, f'entity2idx.pkl')
        self.entity2idx = pickle.load(open(self.entity2idx_path, "rb"))
        self.relation2idx_path = os.path.join(DATA_DIR, self.dataset_dir, f'relation2idx.pkl')
        self.relation2idx = pickle.load(open(self.relation2idx_path, "rb"))
        self.ent_labels = list(self.entity2idx.keys())
        if self.numerical:
            self.three_hop_dict_label_path = os.path.join(DATA_DIR, self.dataset_dir, 'three_hop_dict_label_numerical')
            self.three_hop_dict_index_path = os.path.join(DATA_DIR, self.dataset_dir, 'three_hop_dict_index_numerical')
        else:
            self.three_hop_dict_label_path = os.path.join(DATA_DIR, self.dataset_dir, 'three_hop_dict_label')
            self.three_hop_dict_index_path = os.path.join(DATA_DIR, self.dataset_dir, 'three_hop_dict_index')
        self.three_hop_dict_label = {}
        self.three_hop_dict_index = {}
        if os.path.exists(self.three_hop_dict_label_path) and os.path.exists(self.three_hop_dict_index_path):
            self.three_hop_dict_label = json.loads(open(self.three_hop_dict_label_path).read())
            print(f"Loading 3 hop dictionary from {self.three_hop_dict_label_path}")
            self.three_hop_dict_index = json.loads(open(self.three_hop_dict_index_path).read())
            print(f"Loading 3 hop dictionary from {self.three_hop_dict_index_path}")

        else:
            self.parse_knowledge_graph()
        self.idx_triples = self.make_idx_triples()
        self.entity_labels = list(self.entity2idx.keys())
        self.relation_labels = list(self.relation2idx.keys())

    def make_idx_triples(self):
        idx_triples = []
        for idx, row in self.triples.iterrows():
            s, p, o = row.values.tolist()
            # translate label to idx
            s = self.entity2idx[s]
            p = self.relation2idx[p]
            o = self.entity2idx[o]
            idx_triples.append('_'.join([str(s), str(p), str(o)]))
        return set(idx_triples)

    def check_triple_existence(self, triple_str):
        return triple_str in self.idx_triples

    def parse_knowledge_graph(self):
        """
        :return: a dictionary mapping node name to a list of neighbours, a dictionary mapping index to a list of
        neighbour's indices
        """

        print(f"Creating 3 hop dictionary.")
        one_hop_dict = {}  # label to label dict
        one_hop_idx_dict = {}  # idx to idx dict
        three_hop_dict = {}  # label to label dict
        three_hop_idx_dict = {}  # idx to idx dict
        counter = 0
        for entity in self.ent_labels:
            if "Reaction" not in entity:
                entity_idx = self.entity2idx[entity]
                neighbour_rows = self.triples[self.triples.isin([entity]).any(axis=1)]
                # extract first neighbours
                neighbours = neighbour_rows.iloc[:, 0].values.tolist() + neighbour_rows.iloc[:, 2].values.tolist()
                if entity in neighbours:
                    neighbours.remove(entity)
                neighbours_idx = [self.entity2idx[n] for n in neighbours]
                one_hop_dict[entity] = neighbours
                one_hop_idx_dict[entity_idx] = neighbours_idx
                counter += 1
                print(f"{counter} out of {len(self.ent_labels)}")

            # for entity in self.ent_labels:
            #     entity_idx = self.entity2idx[entity]
            #     first_neighbours = one_hop_dict[entity]
            #     second_neighbours = []
            #     third_neighbours = []
            #     for first_neighbour in first_neighbours:
            #         second_neighbours += one_hop_dict[first_neighbour]
            #         for second_neighbour in second_neighbours:
            #             third_neighbours += one_hop_dict[second_neighbour]

                # three_hop_dict[entity] = list(set(first_neighbours + second_neighbours + third_neighbours))
                three_hop_dict[entity] = list(set(neighbours))
                if entity in three_hop_dict[entity]:
                    three_hop_dict[entity].remove(entity)
                three_hop_idx_dict[entity_idx] = [self.entity2idx[e_idx] for e_idx in three_hop_dict[entity]]

        self.three_hop_dict_label = three_hop_dict
        self.three_hop_dict_index = three_hop_idx_dict

        with open(self.three_hop_dict_label_path, 'w') as f:
            f.write(json.dumps(self.three_hop_dict_label))
            f.close()
        print(f'Writing label dictionary to {self.three_hop_dict_index_path}')

        with open(self.three_hop_dict_index_path, 'w') as f:
            f.write(json.dumps(self.three_hop_dict_index))
            f.close()
        print(f'Writing index dictionary to {self.three_hop_dict_index_path}')

    def extract_neighbour_from_idx(self, entity_idx):
        if str(entity_idx) in self.three_hop_dict_index:
            return list(set(self.three_hop_dict_index[str(entity_idx)]))
        else:
            return None


if __name__ == "__main__":
    START_TIME = time.time()

    import argparse
    argParser = argparse.ArgumentParser()
    argParser.add_argument("-onto", "--ontology", type=str, help="the name of your ontology")
    argParser.add_argument("-dir", "--directory", type=str, help="the directory of your ontology")
    args = argParser.parse_args()
    _ontology = args.ontology
    _dataset_dir = args.directory

    # _ontology = "fb15k"
    # _dataset_dir = f"CrossGraph/{_ontology}"
    my_extractor = HopExtractor(dataset_dir=os.path.join(DATA_DIR, _dataset_dir),
                                dataset_name=_ontology)

    print(time.time() - START_TIME)
