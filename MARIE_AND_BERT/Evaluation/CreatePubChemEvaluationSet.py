import json
import random

import pandas as pd
import csv
import os
import pickle
from Marie.Util.location import DATA_DIR


class CreatePubChemEvaluationSet:

    def __init__(self):
        self.ontology = "pubchem"
        self.dataset_dir = os.path.join(DATA_DIR, 'CrossGraph/pubchem')
        self.DICTIONARY_PATH = os.path.join(DATA_DIR, 'Dictionaries/pubchem/name_dict.json')
        self.ent_label_path = os.path.join(self.dataset_dir, r'ent_labels.tsv')
        self.full_entity_list = [e.strip() for e in open(self.ent_label_path).readlines()]
        self.r2i_path = open(os.path.join(self.dataset_dir, 'relation2idx.pkl'), 'rb')
        self.rel2idx = pickle.load(self.r2i_path)
        self.pubchem_csv_path = os.path.join(DATA_DIR, 'pubchem.csv')
        self.pubchem = csv.DictReader(open(self.pubchem_csv_path).readlines()[0:50])
        self.species_name_mapping = {}
        self.entity_list = []
        self.stop_list = []
        self.how_many_relations = []
        self.normal_relations = []
        self.make_species_name_mapping()
        self.iri_dict = json.loads(open(self.DICTIONARY_PATH).read())

        self.synonym = {"log_p": ["Partition coefficient"],
                        "inchi_standard": ["International Chemical Identifier standard", "standard inchi string",
                                           "standard inchi"],
                        "charge": ["charge number", "electronic charge", "ionic charge"],
                        "molecular_formula": ["formula", "chemical formula", "compound formula"],
                        "canonical_smiles": ["smiles string", "smiles format",
                                             "simplified molecular-input line-entry system"],
                        "molecular_weight": ["weight", "mass", "molecular mass"],
                        "tpsa": ["topological polar surface area", "polar surface area"],
                        "inchi_key": ["InChIKey"],
                        "compound_complexity": ["complexity"]
                        }
        self.triples = pd.read_csv(os.path.join(self.dataset_dir, f"{self.ontology}-train.txt"), header=None,
                                   index_col=None,
                                   sep="\t")

    def check_triple_exist(self, head, rel):
        df = self.triples
        rows = df.loc[(df.iloc[:, 0] == head) & (df.iloc[:, 1] == rel)]
        rst = rows.iloc[:, 2].tolist()
        return rst

    def make_species_name_mapping(self):
        for row in self.pubchem:
            short_iupac_names = []
            formula_list = []
            iupac_name = row['iupac_name']
            formula = row['molecular_formula']
            CID = row['compound_id']
            self.entity_list.append(CID)
            if len(iupac_name) < 20:
                short_iupac_names.append(row['iupac_name'])
            formula_list.append(formula)
            name_list_for_one_species = short_iupac_names + formula_list
            name_list_for_one_species = [n for n in name_list_for_one_species if n.strip() != '']
            self.species_name_mapping[CID] = name_list_for_one_species

        headers = self.pubchem.fieldnames
        for head in headers:
            if head not in self.stop_list:
                if 'count_' in head:
                    r = head.replace('count_', '').replace('_', ' ')
                    self.how_many_relations.append((r, head))
                else:
                    r = head.replace('_', ' ')
                    self.normal_relations.append((r, head))

    def run(self):
        full_relation_label_list = []
        questions_for_cross_graph_training = []
        for h_r, r_iri in self.how_many_relations:
            t_1 = '''how many %s are in %s'''
            t_2 = '''what is the number of %s of %s'''
            full_relation_label = 'count_' + h_r.replace(' ', '_')
            full_relation_label_list.append(full_relation_label)
            for _CID in self.species_name_mapping:
                for name in self.species_name_mapping[_CID]:
                    if name in self.iri_dict:
                        _CID = self.iri_dict[name]
                    tail_entity = _CID + '_' + full_relation_label
                    if tail_entity in self.full_entity_list:
                        q_1 = (t_1 % (h_r, name), _CID, 0, tail_entity.replace('\n', ''), r_iri)
                        q_2 = (t_2 % (h_r, name), _CID, 0, tail_entity.replace('\n', ''), r_iri)
                        questions_for_cross_graph_training = questions_for_cross_graph_training + [q_1, q_2]
                        self.entity_list.append(tail_entity)

        for r, r_iri in self.normal_relations:
            labels = [r]
            if r in self.synonym:
                # TODO: find alternative labels of relation
                other_r = self.synonym[r]
                labels = labels + other_r
            t_1 = '''what is the %s of %s'''
            t_2 = '''what is %s's %s'''
            full_relation_label = r.replace(' ', '_')
            full_relation_label_list.append(full_relation_label)
            for r_label in labels:
                for _CID in self.species_name_mapping:
                    # TODO: replace _CID according to the dictionary value
                    for name in self.species_name_mapping[_CID]:
                        if name in self.iri_dict:
                            _CID = self.iri_dict[name]
                        tail_entity = _CID + '_' + full_relation_label
                        if tail_entity in self.full_entity_list:
                            self.entity_list.append(tail_entity)
                            q_1 = (t_1 % (r_label, name), _CID, 0, tail_entity, name, r_iri)
                            q_2 = (t_2 % (name, r_label), _CID, 0, tail_entity, name, r_iri)
                            questions_for_cross_graph_training = questions_for_cross_graph_training + [q_1, q_2]

        questions_for_cross_graph_training = list(set(questions_for_cross_graph_training))
        df_pubchem_test_set = pd.DataFrame(questions_for_cross_graph_training)
        df_pubchem_test_set.columns = ["question", "head", "domain", "answer", "mention", "relation"]
        df_pubchem_test_set.to_csv(os.path.join(self.dataset_dir, 'pubchem_test.tsv'), sep='\t', index=False)
        full_relation_label_list = list(set(full_relation_label_list))
        with open(os.path.join(self.dataset_dir, "pubchem_relations"), "w") as outfile:
            outfile.write("\n".join(full_relation_label_list))


if __name__ == "__main__":
    my_pubchem_evaluation_creator = CreatePubChemEvaluationSet()
    my_pubchem_evaluation_creator.run()
