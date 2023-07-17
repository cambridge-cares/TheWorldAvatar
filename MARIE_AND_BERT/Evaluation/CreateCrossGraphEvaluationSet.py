import json
import os

import pandas as pd

from CreateOntoKinEvaluationSet import CreateOntoKinEvaluationSet
from CreateOntoCompChemEvaluationSet import CreateOntoCompChemEvaluationSet
from CreateOntoSpeciesEvaluationSet import CreateOntoSpeciesEvaluationSet
from CreatePubChemEvaluationSet import CreatePubChemEvaluationSet

from Marie.Util.location import DATA_DIR


def create_question(question_dict):
    question_list = []
    ontologies = list(question_dict.keys())
    head_iris = []
    tails_iris = []
    q_1, q_2 = "", ""
    species = ""
    for ontology, row in question_dict.items():
        species, head_iri, rel_iri, rel_type, tails = row
        tail = tails[0]
        tails_iris.append(tail)
        t_1 = '''what is the %s of %s'''
        t_2 = '''what is %s's %s'''
        q_1 = t_1 % (rel_type, species)
        q_2 = t_2 % (species, rel_type)
        head_iris.append(head_iri)

    row_1 = (q_1, head_iris, ontologies, tails_iris, species)
    row_2 = (q_2, head_iris, ontologies, tails_iris, species)
    return [row_1, row_2]


class CreateCrossGraphEvaluationSet:

    def __init__(self):
        self.DICTIONARY_DIR = os.path.join(DATA_DIR, 'Dictionaries')
        self.dataset_dir = os.path.join(DATA_DIR, "CrossGraph")
        self.ontologies = ["ontocompchem", "ontokin", "pubchem", "ontospecies"]
        self.rel_mapping = {"smiles": {"pubchem": "canonical_smiles", "ontospecies": "SMILES"},
                            "geometry": {"ontocompchem": "latent_relation_5",
                                         "ontokin": "SpeciesGeometry_latent",
                                         "ontospecies": "hasGeometry"},
                            "molecular weight": {"pubchem": "molecular_weight", "ontospecies": "hasMolecularWeight"},
                            "charge": {"ontospecies": "hasCharge", "pubchem": "charge"}
                            }

        overlapping_species_path = os.path.join(self.DICTIONARY_DIR, 'overlapping_species')
        self.overlapping_species = json.loads(open(overlapping_species_path).read())
        self.ontospecies_creator = CreateOntoSpeciesEvaluationSet()
        self.ontokin_creator = CreateOntoKinEvaluationSet()
        self.ontocompchem_creator = CreateOntoCompChemEvaluationSet()
        self.pubchem_creator = CreatePubChemEvaluationSet()

        self.creator_dict = {"ontospecies": self.ontospecies_creator, "ontokin": self.ontokin_creator,
                             "ontocompchem": self.ontocompchem_creator, "pubchem": self.pubchem_creator}

        # TODO: find all the questions that have cross graph answers and find the correct answers

    def run(self):
        question_list = []
        for rel_type, rel_mapping in self.rel_mapping.items():
            ontologies = list(rel_mapping.keys())
            for species, species_ontologies in self.overlapping_species.items():
                question_dict = {}
                if set(ontologies).issubset(set(species_ontologies)):
                    head_iri_dict = self.find_head_iri(species, rel_mapping)
                    for ontology, head_iri in head_iri_dict.items():
                        rel_iri = rel_mapping[ontology]
                        tails = self.get_triple_tails(head=head_iri, rel=rel_iri, ontology=ontology)
                        row = (species, head_iri, rel_iri, rel_type, tails)
                        if len(tails) >= 1:
                            question_dict[ontology] = row

                    questions = create_question(question_dict)
                    question_list += questions

        df = pd.DataFrame(question_list)
        df.columns = ["question", "heads", "domains", "answers", "mention"]
        df.to_csv(os.path.join(self.dataset_dir, "cross_graph_test.tsv"), sep='\t')

    def find_head_iri(self, species, rel_mapping):
        rst = {}
        for ontology, rel_iri in rel_mapping.items():
            creator = self.creator_dict[ontology]
            species_upper = species.upper()
            species_iri = None
            if species in creator.iri_dict:
                species_iri = creator.iri_dict[species]
            elif species_upper in creator.iri_dict:
                species_iri = creator.iri_dict[species_upper]
            rst[ontology] = species_iri
        return rst

    def get_triple_tails(self, head, rel, ontology):
        creator = self.creator_dict[ontology]
        tails = creator.check_triple_exist(head, rel)
        return tails

    def find_over_lapping_species(self):
        # Find species that appears in more than one ontology ...
        overlapping_species = {}
        name_list_dict = {}
        for ontology in self.ontologies:
            name_list = json.loads(open(os.path.join(self.DICTIONARY_DIR, ontology, "name_list.json")).read())
            name_list_dict[ontology] = name_list
            print(f"Done with {ontology}")
        for ontology, name_list in name_list_dict.items():
            print(f"Starting {ontology}")
            for ontology_2, name_list_2 in name_list_dict.items():
                if ontology != ontology_2:
                    for name_1 in name_list:
                        for name_2 in name_list_2:
                            if name_1.lower().strip() == name_2.lower().strip():
                                if name_1.lower().strip() in overlapping_species:
                                    overlapping_species[name_1.lower().strip()].append(ontology)
                                    overlapping_species[name_1.lower().strip()] = \
                                        list(set(overlapping_species[name_1.lower().strip()]))
                                else:
                                    overlapping_species[name_1.lower().strip()] = [ontology]
            print(f"Done with {ontology}")

        with open(os.path.join(self.DICTIONARY_DIR, 'overlapping_species'), 'w') as f:
            f.write(json.dumps(overlapping_species, indent=4))
            f.close()


if __name__ == "__main__":
    my_creator = CreateCrossGraphEvaluationSet()
    my_creator.run()
