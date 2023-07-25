import hashlib
import json
import os
import pickle

import pandas as pd
from SPARQLWrapper import SPARQLWrapper, JSON
from Marie.Util.Web.SPARQLWarehouse import FIND_ALL_REACTIONS, FIND_SAMPLE_REACTIONS
from Marie.Util.location import DATA_DIR
from KGToolbox import Utils as KGTools


class OntoKinReactionReader:

    def __init__(self):
        self.dataset_path = "CrossGraph/ontokin_reactions"
        self.unique_equations_path = os.path.join(DATA_DIR, self.dataset_path, 'unique_equations.json')
        if os.path.exists(self.unique_equations_path):
            self.unique_equations = json.loads(open(self.unique_equations_path).read())
        else:
            self.unique_equations = []

        self.unique_reactions_path = os.path.join(DATA_DIR, self.dataset_path, 'unique_reactions.json')
        if os.path.exists(self.unique_reactions_path):
            self.unique_reactions = json.loads(open(self.unique_reactions_path).read())
        else:
            self.unique_reactions = []

        self.unique_equation_parts = []
        self.unique_species = []
        self.hashed_unique_equations = []
        self.hashed_unique_equation_parts = []
        self.hashed_unique_species = []
        self.value_dictionary = {}
        self.triples = []
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.entity2idx_path = os.path.join(DATA_DIR, self.dataset_dir, f'entity2idx.pkl')
        self.entity2idx = pickle.load(open(self.entity2idx_path, "rb"))
        self.three_hop_dict_label_path = os.path.join(DATA_DIR, self.dataset_dir, 'three_hop_dict_label')
        self.three_hop_dict_index_path = os.path.join(DATA_DIR, self.dataset_dir, 'three_hop_dict_index')
        self.three_hop_dict_label = {}
        self.three_hop_dict_index = {}

    def append_value_dict(self, node, value):
        if node in self.three_hop_dict_label:
            self.three_hop_dict_label[node].append(value)
        else:
            self.three_hop_dict_label[node] = [value]

        hashed_part_idx = self.entity2idx[node]
        if hashed_part_idx in self.three_hop_dict_index:
            self.three_hop_dict_index[hashed_part_idx].append(self.entity2idx[value])
        else:
            self.three_hop_dict_index[hashed_part_idx] = [self.entity2idx[value]]

    def split_equations(self, short_reaction, equation):
        splitted_equation = [p.strip() for p in equation.split('[=]')]
        for part in splitted_equation:
            part_idx = splitted_equation.index(part)
            hashed_part = self.make_hash(part)
            # if short_reaction in self.unique_reactions:
            #     self.append_value_dict(hashed_part, short_reaction)

            if part_idx == 0:
                # this is the reactants
                self.triples.append((hashed_part, 'isReactant', short_reaction))

            elif part_idx == 1:

                self.triples.append((hashed_part, 'isProducts', short_reaction))
            if hashed_part not in self.value_dictionary:
                self.value_dictionary[hashed_part] = part
            species_list = [s.strip() for s in part.split('+')]
            for species in species_list:
                hashed_species = self.make_hash(species)
                # if short_reaction in self.unique_reactions:
                #     self.append_value_dict(hashed_species, short_reaction)
                if part_idx == 0:
                    self.triples.append((hashed_part, 'isReactant', short_reaction))
                elif part_idx == 1:
                    self.triples.append((hashed_species, 'isProducts', short_reaction))
                if hashed_species not in self.value_dictionary:
                    self.value_dictionary[hashed_species] = species

    def make_hash(self, value):
        value = value.encode('utf-8')
        return str(hashlib.sha256(value).hexdigest())

    def update_dict(self):
        # DONE: load the dictionary if there is any
        if os.path.exists(self.three_hop_dict_label_path):
            loaded_dict = json.loads(open(self.three_hop_dict_label_path).read())
        else:
            loaded_dict = {}
        # DONE: update the loaded dict with current data
        loaded_dict.update(self.three_hop_dict_label)
        # DONE: write the updated dict to the file
        with open(self.three_hop_dict_label_path, 'w') as f:
            f.write(json.dumps(loaded_dict))
            f.close()

        self.three_hop_dict_label = {}

    def find_all_unique_reactions(self):
        all_reactions = KGTools.query_blazegraph(FIND_ALL_REACTIONS, namespace="ontokin")
        all_reactions = all_reactions['results']['bindings']
        counter = 0
        for row in all_reactions:
            counter += 1
            reaction = row['reaction']['value']
            equation = row['equation']['value']
            equation = equation.replace('(', '').replace(')', '').replace(' =]', ' [=]').strip()
            short_reaction = reaction.split('/')[-1].strip()
            if equation not in self.unique_equations:
                self.unique_reactions.append(short_reaction)

        self.unique_reactions = list(set(self.unique_reactions))

        with open(os.path.join(DATA_DIR, self.dataset_path, 'unique_reactions.json'), 'w') as f:
            f.write(json.dumps(self.unique_reactions))
            f.close()

    def create_triples(self):
        pass

    def make_reaction_equation_pairs(self):
        all_reactions = KGTools.query_blazegraph(FIND_ALL_REACTIONS, namespace="ontokin")
        all_reactions = all_reactions['results']['bindings']
        counter = 0
        for row in all_reactions:
            counter += 1
            if counter % 5000 == 0:
                self.update_dict()
            print(f"{counter} out of {len(all_reactions)}")
            reaction = row['reaction']['value']
            equation = row['equation']['value']
            equation = equation.replace('(', '').replace(')', '').replace(' =]', ' [=]').strip()
            short_reaction = reaction.split('/')[-1].strip()
            if short_reaction in self.unique_reactions:
                self.split_equations(short_reaction, equation)
                hashed_equation = self.make_hash(equation)
                self.value_dictionary[short_reaction] = equation
            # self.triples.append((short_reaction, 'hasEquation', hashed_equation))

        df = pd.DataFrame(self.triples)
        df.to_csv(os.path.join(DATA_DIR, self.dataset_path, f'ontokin_reactions-train.txt'),
                  header=False, sep='\t', index=False)
        df_test = df.sample(frac=0.2)
        df_test.to_csv(os.path.join(DATA_DIR, self.dataset_path, f'ontokin_reactions-test.txt'),
                       header=False, sep='\t', index=False)

        with open(os.path.join(DATA_DIR, self.dataset_path, 'ontokin_reactions_value_dict.json'), 'w') as f:
            f.write(json.dumps(self.value_dictionary))
            f.close()

        with open(self.three_hop_dict_label_path, 'w') as f:
            f.write(json.dumps(self.three_hop_dict_label))
            f.close()
        print(f'Writing label dictionary to {self.three_hop_dict_index_path}')

        with open(self.three_hop_dict_index_path, 'w') as f:
            f.write(json.dumps(self.three_hop_dict_index))
            f.close()
        print(f'Writing index dictionary to {self.three_hop_dict_index_path}')


if __name__ == '__main__':
    my_reader = OntoKinReactionReader()
    my_reader.find_all_unique_reactions()
    my_reader.make_reaction_equation_pairs()
