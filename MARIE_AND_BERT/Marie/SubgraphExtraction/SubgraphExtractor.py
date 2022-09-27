import logging
import os
import pickle

from Marie.Util.location import DEPLOYMENT_DIR

logging.basicConfig(level=logging.DEBUG, filename='marie.log', format='%(asctime)s %(levelname)s:%(message)s')

# we need a proper place to hold all the triples, currently in file form

class SubgraphExtractor:
    def __init__(self, dataset_name='pubchem500'):
        self.PUBCHEM_PATH = os.path.join(DEPLOYMENT_DIR, f'{dataset_name}-train.txt')
        e2i_file = open(os.path.join(DEPLOYMENT_DIR, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_file)

        self.entity_dictionary = {}
        self.pubchem_triples = []
        self.load_pubchem()
        self.make_dictionary()
        pubchem_value_dict_path = open(os.path.join(DEPLOYMENT_DIR, 'pubchem_value_dict.pkl'), 'rb')
        self.pubchem_value_dict = pickle.load(pubchem_value_dict_path)

    def load_pubchem(self):
        try:
            self.pubchem_triples = open(self.PUBCHEM_PATH).readlines()
            return self.PUBCHEM_PATH
        except:
            logging.error(f"Error loading the pubchem data")

    # make a mapping between head entities and their related tail entities
    def make_dictionary(self):
        tmp = {}
        for triple in self.pubchem_triples:
            entities = triple.split('\t')
            head_entity = entities[0].strip()
            tail_entity = entities[2].strip()
            if head_entity in tmp:
                tmp[head_entity].append(self.entity2idx[tail_entity])
            else:
                tmp[head_entity] = [self.entity2idx[tail_entity]]
        self.entity_dictionary['pubchem'] = tmp

    def retrieve_subgraph(self, _head_entity):
        return self.entity_dictionary['pubchem'][_head_entity]

    def value_lookup(self, node_name):
        if node_name in self.pubchem_value_dict:
            return self.pubchem_value_dict[node_name]
        else:
            return 'Node with no value'


if __name__ == '__main__':
    se = SubgraphExtractor(dataset_name='pubchem50000')
    rst = se.retrieve_subgraph('CID1')
    print(rst)
