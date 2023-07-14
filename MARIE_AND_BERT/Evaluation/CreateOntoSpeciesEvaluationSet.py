import json, os
import random

import pandas as pd

from Marie.Util.location import DICTIONARY_DIR, DATA_DIR


class CreateOntoSpeciesEvaluationSet:

    def __init__(self):
        self.dataset_dir = os.path.join(DATA_DIR, 'CrossGraph/ontospecies')
        self.overlapping_species_path = os.path.join(DICTIONARY_DIR, 'overlapping_species')
        self.overlapping_species = json.loads(open(self.overlapping_species_path).read())
        self.selected_species = self.select_overlapping_species()
        self.triples = pd.read_csv(os.path.join(self.dataset_dir, "ontospecies-train.txt"), header=None, index_col=None,
                                   sep="\t")
        self.properties_and_labels = pd.read_csv(os.path.join(self.dataset_dir, "species_properties_labels.tsv"),
                                                 sep="\t")
        self.iri_dict = json.loads(open(os.path.join(DICTIONARY_DIR, "ontospecies/name_dict.json")).read())
        self.properties_and_labels["labels"] = self.properties_and_labels["labels"].apply(eval)
        """
        casRegistryID
        inChI
        SMILES
        hasCharge
        hasMolecularWeight
        hasMolecularFormula
        hasAtom
        hasGeometry
        hasStandardEnthalpyOfFormation
        pubChemCID
        spinMultiplicity
        """

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
                for idx, row in self.properties_and_labels.iterrows():
                    rel, labels = row
                    tails = self.check_triple_exist(iri, rel)
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
        df.to_csv(os.path.join(self.dataset_dir, 'ontospecies_test.tsv'), sep='\t', index=False)

    def select_overlapping_species(self):
        selected_species = []
        for key, item in self.overlapping_species.items():
            if "ontospecies" in item:
                selected_species.append(key)
        return selected_species


if __name__ == "__main__":
    my_creator = CreateOntoSpeciesEvaluationSet()
    my_creator.run()
