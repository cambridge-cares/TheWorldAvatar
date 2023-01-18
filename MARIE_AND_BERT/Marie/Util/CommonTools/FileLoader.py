import json
import os
import pickle

from Marie.Util.Logging import MarieLogger
from Marie.Util.location import DATA_DIR


class FileLoader:
    """
    File loader loads embedding files, index files and so on
    """

    def __init__(self, full_dataset_dir, dataset_name):
        self.marie_logger = MarieLogger()
        self.full_dataset_dir = full_dataset_dir
        self.dataset_name = dataset_name
        self.marie_logger.info(f"Loading files from {self.full_dataset_dir}")

    def load_value_dict(self, dict_type):
        value_dictionary_path = os.path.join(f"{self.full_dataset_dir}/{self.dataset_name}_value_dict.{dict_type}")
        if dict_type == 'json':
            value_dictionary = json.loads(open(value_dictionary_path).read())
        else:
            file = open(value_dictionary_path, 'rb')
            value_dictionary = pickle.load(file)
        return value_dictionary

    def load_all_triples(self):
        triple_path = os.path.join(f'{self.full_dataset_dir}/{self.dataset_name}-train.txt')
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
        all_species_indices_file = open(f'{self.full_dataset_dir}/all_species.pkl', 'rb')
        all_species_indices = pickle.load(all_species_indices_file)
        return all_species_indices


if __name__ == "__main__":
    dataset_dir = "CrossGraph/wikidata_numerical"
    dataset_name = "wikidata_numerical"
    file_loader = FileLoader(full_dataset_dir=os.path.join(DATA_DIR, dataset_dir),
                             dataset_name=dataset_name)
    all_heads = file_loader.load_all_heads_tensor()
    print(all_heads)
