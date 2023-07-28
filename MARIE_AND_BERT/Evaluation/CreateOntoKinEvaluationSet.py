import json
import os
import random

import pandas as pd

from Marie.Util.location import DICTIONARY_DIR
from Marie.Util.location import DATA_DIR


class CreateOntoKinEvaluationSet:

    def __init__(self):
        self.ontology = "ontokin"
        self.property_dict = \
            {"SpeciesGeometry_latent": ["geometry"],
             "RotationalRelaxationCollisionNumber_latent": ["rotational relaxation collision number"],
             "Polarizability_latent": ["polarizability"],
             "LennardJonesDiameter_latent": ["lennard jones diameter"],
             "LennardJonesWellDepth_latent": ["lennard jones well depth"],
             "DipoleMoment_latent": ["dipole moment"]
             }

        self.dataset_dir = os.path.join(DATA_DIR, f'CrossGraph/{self.ontology}')
        self.overlapping_species_path = os.path.join(DICTIONARY_DIR, 'overlapping_species')
        self.overlapping_species = json.loads(open(self.overlapping_species_path).read())
        self.selected_species = self.select_overlapping_species()
        self.triples = pd.read_csv(os.path.join(self.dataset_dir, f"{self.ontology}-train.txt"), header=None,
                                   index_col=None,
                                   sep="\t")
        self.iri_dict = json.loads(open(os.path.join(DICTIONARY_DIR, f"{self.ontology}/name_dict.json")).read())

    def select_overlapping_species(self):
        selected_species = []
        for key, item in self.overlapping_species.items():
            if self.ontology in item:
                selected_species.append(key)
        return selected_species

    def check_triple_exist(self, head, rel):
        df = self.triples
        rows = df.loc[(df.iloc[:, 0] == head) & (df.iloc[:, 1] == rel)]
        rst = rows.iloc[:, 2].tolist()
        return rst

    def run(self):
        t_1 = '''what is the %s of %s'''
        t_2 = '''what is %s's %s'''
        all_questions = []
        for species in random.sample(self.selected_species, 50):
            species = species.upper()
            if species in self.iri_dict:
                iri = self.iri_dict[species]
                for key, value in self.property_dict.items():
                    rel, labels = key, value
                    tails = self.check_triple_exist(iri, rel)
                    if len(tails) > 1:
                        print("multiple tails", tails)
                        print(len(tails))
                    if len(tails) == 1:
                        tail = tails[0]
                        for label in labels:
                            # ["question", "head", "domain", "answer", "mention", "relation"]
                            q_1 = t_1 % (label, species)
                            q_2 = t_2 % (species, label)
                            row_1 = (q_1, iri, 2, tail, species, rel)
                            row_2 = (q_2, iri, 2, tail, species, rel)
                            all_questions = all_questions + [row_1, row_2]
        df = pd.DataFrame(all_questions)
        df.columns = ["question", "head", "domain", "answer", "mention", "relation"]
        df.to_csv(os.path.join(self.dataset_dir, f'{self.ontology}_test.tsv'), sep='\t', index=False)


if __name__ == "__main__":
    my_creator = CreateOntoKinEvaluationSet()
    my_creator.run()
