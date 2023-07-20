import json
import os
import random

import numpy as np
import pandas as pd

from Marie.Util.location import DATA_DIR


class OntoKinReactionSplitter:
    """
    Due to the sheer size of OntoKin reaction part, it is hardly possible to embed all the reactions
    as a whole. Therefore, this class attempts to divide the species - reaction pairs

    Number of unique species = 88556
    Number of triples = 568283
    On average, each species takes part in about 6 reactions

    Attempt 1: divide the dataset into 50 parts, take one to test ...
    """

    def __init__(self, ontology="ontokin_reaction", split_num=20):
        self.ontology = ontology
        self.split_num = split_num
        self.dataset_dir = f"CrossGraph/{self.ontology}"
        self.triples = self.read_triple()
        self.unique_species = []
        self.all_triples_list = []


    def find_unrelated_reaction(self):
        """
        For one reaction, it is highly likely that there exist some reactions that have no reactants/products
        in common with it ...
        Iterate the KG, randomly select a set of reactions, find all the species that are 
        :return:
        """



    def split_triples_by_species(self):
        unique_species_path = os.path.join(DATA_DIR, "CrossGraph/ontokin_reactions", "unique_species.json")
        if os.path.exists(unique_species_path):
            self.unique_species = json.loads(open(unique_species_path).read())
        else:
            counter = 0
            for triple in self.triples:
                counter += 1
                print(f"{counter} out of {len(self.triples)}")
                s, p, o = [e.strip() for e in triple.split("\t")]
                if s not in self.unique_species:
                    self.unique_species.append(s)
            with open(unique_species_path, "w") as f:
                f.write(json.dumps(self.unique_species))
                f.close()

        # Shuffle the species list
        random.shuffle(self.unique_species)
        splitted_species = np.array_split(self.unique_species, self.split_num)
        # TODO: then find all triples connected to one species list
        counter = 0
        splitted_triples = []
        for triple in self.triples:
            counter += 1
            print(f"{counter} out of {len(self.triples)}")
            s, p, o = [e.strip() for e in triple.split("\t")]
            if s in splitted_species[0]:
                splitted_triples.append((s, p, o))

        df_train = pd.DataFrame(splitted_triples)
        df_train.to_csv(os.path.join(DATA_DIR, f"CrossGraph/{self.ontology}", f"{self.ontology}-train.txt"),
                        sep="\t", header=False, index=False)

        df_test = df_train.sample(frac=0.2)
        df_valid = df_train.sample(frac=0.2)
        df_test.to_csv(os.path.join(DATA_DIR, f"CrossGraph/{self.ontology}", f"{self.ontology}-test.txt"),
                       sep="\t", header=False, index=False)
        df_valid.to_csv(os.path.join(DATA_DIR, f"CrossGraph/{self.ontology}", f"{self.ontology}-valid.txt"),
                        sep="\t", header=False, index=False)

    def read_triple(self):
        triple_path = os.path.join(DATA_DIR, "CrossGraph/ontokin_reactions", f"ontokin_reactions-train.txt")
        return open(triple_path).readlines()

    def run(self):
        self.split_triples_by_species()


if __name__ == "__main__":
    my_splitter = OntoKinReactionSplitter(ontology="ontokin_reactions_test_40", split_num=40)
    my_splitter.run()
