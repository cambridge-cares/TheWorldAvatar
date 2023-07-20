import os
import sys
import random

from KGToolbox.Tools import MakeIndex

sys.path.append("../..")
import json
import time

import pandas as pd
from wikidata.client import Client
from wikidata.entity import EntityId
from Marie.Util.location import DATA_DIR


def load_file(file_dir, file_name, mode):
    file_path = os.path.join(file_dir, file_name)
    if os.path.exists(file_path):
        return json.loads(open(file_path).read())
    else:
        if mode == "dict":
            return {}
        else:
            return []


class WikiDataReader:

    def __init__(self, dataset_name="wikidata_numerical"):
        self.client = Client()
        self.identifier_relations = ['P274', 'P233']
        self.SUB = str.maketrans("₀₁₂₃₄₅₆₇₈₉", "0123456789")
        self.dataset_name = dataset_name
        self.dataset_dir = os.path.join(DATA_DIR, f'CrossGraph/{dataset_name}')
        self.dictionary_dir = os.path.join(DATA_DIR, f'Dictionaries/{dataset_name}')
        # self.collected_species = load_file(file_dir=self.dataset_dir, file_name='log', mode='list')
        self.collected_species = []
        self.unique_relations = load_file(file_dir=self.dataset_dir, file_name='unique_relations.json', mode='list')

        self.name_list = load_file(file_dir=self.dictionary_dir, file_name="name_list.json", mode='list')
        self.name_dict = load_file(file_dir=self.dictionary_dir, file_name="name_dict.json", mode='dict')
        self.files = self.load_entity_list()
        self.triples = load_file(file_dir=self.dataset_dir, file_name="triples.json", mode='list')
        # self.df = pd.read_csv(os.path.join(self.dataset_dir, "wikidata-train.txt"),
        #                       header=None, index_col=None, sep='\t')
        # self.triples = list(self.df.itertuples(index=False))
        # self.value_dict = load_file(file_dir=self.dataset_dir, file_name="wikidata_value_dict.json", mode='dict')
        self.value_dict = {}
        self.repeated_hash = load_file(file_dir=self.dataset_dir, file_name="repeated_hash.json", mode='list')
        self.failed_species = []

    def write_files(self):

        with open(os.path.join(DATA_DIR, f'Dictionaries/{self.dataset_name}', 'name_dict.json'), 'w', encoding='utf-8') as f:
            f.write(json.dumps(self.name_dict))
            f.close()

        with open(os.path.join(DATA_DIR, f'Dictionaries/{self.dataset_name}', 'name_list.json'), 'w', encoding='utf-8') as f:
            f.write(json.dumps(self.name_list))
            f.close()

        with open(os.path.join(self.dataset_dir, f'{self.dataset_name}_value_dict.json'), 'w',
                  encoding='utf-8') as f:
            f.write(json.dumps(self.value_dict))
            f.close()

        with open(os.path.join(self.dataset_dir, 'repeated_hash.json'), 'w',
                  encoding='utf-8') as f:
            f.write(json.dumps(list(set(self.repeated_hash)), indent=4))
            f.close()

        with open(os.path.join(self.dataset_dir, 'failed_species.json'), 'w',
                  encoding='utf-8') as f:
            f.write(json.dumps(list(set(self.failed_species)), indent=4))
            f.close()

        df_train = pd.DataFrame(self.triples)
        df_test = df_train.sample(frac=0.2)
        df_train.to_csv(os.path.join(self.dataset_dir, f"{self.dataset_name}-train.txt"), sep='\t', index=False,
                        header=False)
        df_test.to_csv(os.path.join(self.dataset_dir, f"{self.dataset_name}-test.txt"), sep='\t', index=False,
                       header=False)

        with open(os.path.join(self.dataset_dir, 'log'), 'w',
                  encoding='utf-8') as f:
            f.write(json.dumps(list(set(self.collected_species))))
            f.close()

        with open(os.path.join(self.dataset_dir, 'unique_relations.json'), 'w',
                  encoding='utf-8') as f:
            f.write(json.dumps(list(set(self.unique_relations))))
            f.close()

    def main(self):
        counter = 0
        START_TIME = time.time()
        for file in self.files:
            counter += 1
            if file not in self.collected_species:
                species_id = file.strip()
                try:
                    entity = self.client.get(EntityId(species_id), load=True)
                    print(f"{counter} out of {len(self.files)}")
                    self.update_name_list_and_dict(entity, species_id)
                    self.get_claims(entity, species_id)
                    self.collected_species.append(file)
                except:
                    self.failed_species.append(file)
            time.sleep(1)
            if counter % 100 == 0:
                self.write_files()
                print(time.time() - START_TIME)

        self.write_files()
    def update_name_list_and_dict(self, entity, species_id):
        aliases = self.get_aliases_and_description(entity)
        self.name_list = self.name_list + aliases
        for name in aliases:
            if name not in self.name_dict:
                self.name_dict[name] = [species_id]
            else:
                self.name_dict[name].append(species_id)

    def load_entity_list(self):
        entity_dir = os.path.join(self.dataset_dir, "instance_info")
        files = [f for f in os.listdir(entity_dir) if f.startswith("Q")]
        return files
        # return random.sample(files, 100)

    def test(self, id):
        entity = self.client.get(EntityId(id), load=True)
        test_prop = self.client.get(EntityId('P18'))
        image = entity[test_prop]
        print(image)
        print(self.get_aliases_and_description(entity=entity))
        self.get_claims(entity=entity, species_id=id)

    def get_relation_label(self, id):
        pass

    def get_aliases_and_description(self, entity):
        """
        Get the aliases from the entity, including labels, alt label, formula, and smiles
        :param entity:
        :return:
        """
        label = str(entity.label)
        aliases_list = entity.attributes['aliases']
        if 'en' in aliases_list:
            aliases = [name['value'] for name in aliases_list['en']]
        else:
            aliases = []

        aliases.append(label)
        # get smiles and chemical formula
        for identifier in self.identifier_relations:
            identifier = self.client.get(EntityId(identifier))
            if identifier in entity:
                value = entity[identifier].translate(self.SUB)
                aliases.append(value)

        aliases = [name for name in aliases if len(name) < 30]
        return list(set(aliases))

    def update_triples(self, head, rel, tail_hash):
        self.triples.append((head, rel, tail_hash))

    def update_value_dict(self, hash, value):
        if hash not in self.value_dict:
            self.value_dict[hash] = value
        else:
            self.repeated_hash.append(hash)

    def get_claims(self, entity, species_id):
        """
        Get the statements of a species ...
        :param entity:
        :return:
        """
        claims = entity.attributes['claims']
        for key, value in claims.items():
            for v in value:
                mainsnak = v['mainsnak']
                hash = mainsnak['hash']
                p = mainsnak['property']
                datavalue = mainsnak['datavalue']
                if p not in self.unique_relations:
                    self.unique_relations.append(p)
                self.update_value_dict(hash, datavalue)
                self.update_triples(species_id, p, hash)


if __name__ == "__main__":
    my_reader = WikiDataReader(dataset_name="wikidata_numerical")
    my_reader.main()
    MakeIndex.create_indexing(dataset_name="wikidata_numerical", data_dir="CrossGraph/wikidata_numerical")

    # my_reader.test("Q2270")
