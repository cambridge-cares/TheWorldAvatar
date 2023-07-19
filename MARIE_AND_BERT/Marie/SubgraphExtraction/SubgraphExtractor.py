
import os
import pickle
from Marie.Util.location import DATA_DIR
from Marie.Util.Logging import MarieLogger


# we need a proper place to hold all the triples, currently in file form

class SubgraphExtractor:
    def __init__(self, dataset_name='pubchem500'):
        self.marie_logger = MarieLogger()
        self.dataset_dir = "CrossGraph/pubchem"
        self.PUBCHEM_PATH = os.path.join(DATA_DIR, self.dataset_dir,f'{dataset_name}-train.txt')
        e2i_file = open(os.path.join(DATA_DIR, self.dataset_dir, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_file)
        self.entity_dictionary = {}
        self.pubchem_triples = []
        self.load_pubchem()
        self.marie_logger.info("3. Done loading pubchem")
        self.make_dictionary()
        self.marie_logger.info("4. Done making dictionary")
        pubchem_value_dict_path = open(os.path.join(DATA_DIR, self.dataset_dir, 'pubchem_value_dict.pkl'), 'rb')
        self.pubchem_value_dict = pickle.load(pubchem_value_dict_path)

    def load_pubchem(self):
        try:
            self.pubchem_triples = open(self.PUBCHEM_PATH).readlines()
            self.marie_logger.info("2. Loading Pubchem")
            return self.PUBCHEM_PATH
        except:
            self.marie_logger.critical(f"Error loading the pubchem data from {__name__}.{self.load_pubchem.__name__}")

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
        print("===============")
        print(_head_entity)
        return self.entity_dictionary['pubchem'][_head_entity]

    def value_lookup(self, node_name):
        if node_name in self.pubchem_value_dict:
            return self.pubchem_value_dict[node_name]
        else:
            return None


if __name__ == '__main__':
    se = SubgraphExtractor(dataset_name='pubchem50000')
    rst = se.retrieve_subgraph('CID1')
    print(rst)
