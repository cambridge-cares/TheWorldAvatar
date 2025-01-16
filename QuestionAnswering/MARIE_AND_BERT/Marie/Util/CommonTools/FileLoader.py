import json
import os
import pickle

from Marie.Util.Logging import MarieLogger
from Marie.Util.location import DATA_DIR



class FileLoader:
    """
    File loader loads embedding files, index files and so on
    """

    @staticmethod
    def create_o_p_dict(triples):
        o_p_dict = {}
        for triple in triples:
            s, p, o = [e.strip() for e in triple.split("\t")]
            o_p_dict[o] = p
        return o_p_dict

    def __init__(self, full_dataset_dir, dataset_name=None):
        self.marie_logger = MarieLogger()
        self.full_dataset_dir = full_dataset_dir
        self.dataset_name = dataset_name
        self.marie_logger.info(f"Loading files from {self.full_dataset_dir}")

    def load_value_dict(self, dict_type, file_name=None):
        if file_name is None:
            value_dictionary_path = os.path.join(f"{self.full_dataset_dir}/{self.dataset_name}_value_dict.{dict_type}")
        else:
            value_dictionary_path = os.path.join(f"{self.full_dataset_dir}/{file_name}")
        if dict_type == 'json':
            value_dictionary = json.loads(open(value_dictionary_path).read())
        else:
            file = open(value_dictionary_path, 'rb')
            value_dictionary = pickle.load(file)
        return value_dictionary

    def load_all_triples(self, full_dataset_dir=None, dataset_name=None):
        if full_dataset_dir is None:
            triple_path = os.path.join(f'{self.full_dataset_dir}/{self.dataset_name}-train.txt')
        else:
            triple_path = os.path.join(f'{full_dataset_dir}/{dataset_name}-train.txt')

        triples = open(triple_path).readlines()
        return triples

    def load_index_files(self):
        """Load pickles for idx - label and label - idx transformation """
        i2e_file = open(f'{self.full_dataset_dir}/idx2entity.pkl', 'rb')
        idx2entity = pickle.load(i2e_file)
        e2i_file = open(f'{self.full_dataset_dir}/entity2idx.pkl', 'rb')
        entity2idx = pickle.load(e2i_file)
        i2r_file = open(f'{self.full_dataset_dir}/idx2relation.pkl', 'rb')
        idx2rel = pickle.load(i2r_file)
        r2i_file = open(f'{self.full_dataset_dir}/relation2idx.pkl', 'rb')
        rel2idx = pickle.load(r2i_file)
        return entity2idx, idx2entity, rel2idx, idx2rel

    def load_all_heads_tensor(self):
        # TODO: load all the class indices, for MoPs, there are MoPs, CBU, Assembly model
        all_heads_indices_file = open(f'{self.full_dataset_dir}/all_heads.pkl', 'rb')
        all_heads_indices = pickle.load(all_heads_indices_file)
        return all_heads_indices
        # all_species_indices_file = open(f'{self.full_dataset_dir}/all_species.pkl', 'rb')
        # all_species_indices = pickle.load(all_species_indices_file)
        # return all_species_indices


if __name__ == "__main__":
    dataset_dir = "CrossGraph/wikidata_numerical"
    dataset_name = "wikidata_numerical"
    file_loader = FileLoader(full_dataset_dir=os.path.join(DATA_DIR, dataset_dir),
                             dataset_name=dataset_name)
    all_heads = file_loader.load_all_heads_tensor()
    # print(all_heads)
