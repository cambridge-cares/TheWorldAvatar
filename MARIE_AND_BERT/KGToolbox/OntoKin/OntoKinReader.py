import json
import os
import pickle
from re import finditer

import pandas
import pandas as pd

from KGToolbox.Tools import MakeIndex
from Marie.Util.Web.SPARQLWarehouse import ONTOKIN_ALL_PROPERTIES_ALL_SPECIES
from Marie.Util.location import DATA_DIR
from KGToolbox.Utils import query_blazegraph

"""
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasDipoleMoment>	
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasDipoleMomentUnits>		
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasLennardJonesDiameter>	
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasLennardJonesDiameterUnits>		
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasLennardJonesWellDepth>		
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasLennardJonesWellDepthUnits>		
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasPolarizability>		
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasPolarizabilityUnits>		
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasRotationalRelaxationCollisionNumber>		
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasRotationalRelaxationCollisionNumberUnits>	-	
<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasSpeciesGeometry> 
"""


def camel_case_split(identifier):
    matches = finditer('.+?(?:(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])|$)', identifier)
    return [m.group(0) for m in matches]


class OntoKinReader:

    def __init__(self):
        self.dataset_path = os.path.join(DATA_DIR, 'CrossGraph', 'ontokin')
        self.entity2idx = None
        self.relation2idx = None

    def query_all_species(self):
        # Use one query to find all the properties fo all species under the type species .
        short_species_list = []
        triples = []
        tmp = []
        value_dictionary = {}
        rst = query_blazegraph(query=ONTOKIN_ALL_PROPERTIES_ALL_SPECIES, namespace="ontokin")
        non_attributes = ['species', 'label', 'transport']
        heads = [h for h in rst['head']['vars'] if h not in non_attributes and '_unit' not in h]
        unique_labels = []
        for r in rst['results']['bindings']:
            row = []
            species = r['species']['value']  # .split('/')[-1]
            transport = r['transport']['value']
            label = r['label']['value']

            if "#" in species:
                short_species = species.split('#')[-1]
            else:
                short_species = species.split('/')[-1]

            # filter, only put findable species ...
            counter = 0
            if label not in unique_labels:
                short_species_list.append(short_species)
                counter += 1
                print(f"number of selected iris", counter)
                row.append(species)
                row.append(label)
                for head in heads:

                    if head in r:
                        data = r[head]['value']
                    else:
                        data = "EMPTY"

                    new_node = head + '_' + short_species
                    row.append(new_node)
                    if head + '_unit' in r:
                        data_unit = r[head + '_unit']['value']
                        value_dictionary[new_node] = data + ' ' + data_unit
                    else:
                        # insert a new node, with part of the species and the relations
                        value_dictionary[new_node] = data
                    triples.append((short_species, head + '_latent', new_node))

                tmp.append(row)
                unique_labels.append(label)

        print('number of unique labels', len(unique_labels))
        df_all_species = pd.DataFrame(tmp)
        df_all_species.columns = ['species', 'label'] + heads
        df_all_species.to_csv(os.path.join(self.dataset_path, 'all_species.tsv'), sep='\t')

        with open(os.path.join(self.dataset_path, 'value_dict.json'), 'w') as f:
            f.write(json.dumps(value_dictionary))
            f.close()

        df_triples = pd.DataFrame(triples)
        df_triples.to_csv(os.path.join(self.dataset_path, 'ontokin-train.txt'), sep='\t', index=False, header=False)
        df_test = df_triples.sample(frac=0.2)
        df_test.to_csv(os.path.join(self.dataset_path, 'ontokin-test.txt'), sep='\t', index=False, header=False)


    def run(self):
        self.query_all_species()
        data_folder = 'CrossGraph/ontokin'
        MakeIndex.create_indexing(data_dir=data_folder, dataset_name="ontokin")
        e2i_path = open(os.path.join(DATA_DIR, f'{data_folder}/entity2idx.pkl'), 'rb')
        r2i_path = open(os.path.join(DATA_DIR, f'{data_folder}/relation2idx.pkl'), 'rb')
        _full_dir = os.path.join(DATA_DIR, f'{data_folder}')
        self.entity2idx = pickle.load(e2i_path)
        self.relation2idx = pickle.load(r2i_path)
        self.create_score_model_set()

    def create_score_model_set(self):
        property_dict = {"SpeciesGeometry_latent": "geometry",
                         "RotationalRelaxationCollisionNumber_latent": "rotational relaxation collision number",
                         "Polarizability_latent": "polarizability",
                         "LennardJonesDiameter_latent": "lennard jones diameter",
                         "LennardJonesWellDepth_latent": "lennard jones well depth",
                         "DipoleMoment_latent": "dipole moment"
                         }

        df_train = pd.read_csv(os.path.join(self.dataset_path, "ontokin-train.txt"), sep='\t', header=None)
        question_list = []
        for idx, row in df_train.iterrows():
            # question	head	tail	rel
            h, r, t = row
            if "_latent" in r:
                h_idx = (self.entity2idx[h])
                r_idx = (self.relation2idx[r])
                t_idx = (self.entity2idx[t])
                q = property_dict[r]
                question_list.append((q, h_idx, t_idx, r_idx))

        df_questions = pd.DataFrame(question_list)
        df_questions.columns = ["question", "head", "tail", "rel"]
        df_questions.to_csv(os.path.join(self.dataset_path, "score_model_training.tsv"), sep='\t')


if __name__ == '__main__':
    my_ontokin_reader = OntoKinReader()
    my_ontokin_reader.run()
