import os
import pickle

import pandas as pd

from Marie.Util.location import DATA_DIR

"""
type
hasGeometryType_latent
oc:hasFrequencies_latent
oc:hasRotationalConstants_latent
oc:hasRotationalSymmetryNumber_latent
casRegistryID
inChI
SMILES
hasCharge
hasMolecularWeight
hasMolecularFormula
hasGeometry
spinMultiplicity
hasAtom
hasAtomicBond
hasStandardEnthalpyOfFormation
pubChemCID
"""


class OntoChemistryReader:
    """The onto chemistry reader reads triples of ontochemistry (ontocompchem + ontospecies)
    and create two datasets: score model set with q,h,t,r
    """

    def __init__(self):
        self.dataset_path = os.path.join(DATA_DIR, '')
        self.dataset_name = "ontospecies"
        self.triples = pd.read_csv(os.path.join(self.dataset_path, self.dataset_name, f'{self.dataset_name}-train.txt')
                                   , sep='\t', header=None)

        e2i_path = open(os.path.join(self.dataset_path, f'{self.dataset_name}/entity2idx.pkl'), 'rb')
        r2i_path = open(os.path.join(self.dataset_path, f'{self.dataset_name}/relation2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_path)
        self.relation2idx = pickle.load(r2i_path)

        # only find species triples
        self.property_names = {"type": "type",
                               "hasGeometryType_latent": "geometry type",
                               "oc:hasFrequencies_latent": "vibration frequency",
                               "oc:hasRotationalConstants_latent": "rotational constants",
                               "oc:hasRotationalSymmetryNumber_latent": "rotational symmetry number ",
                               "casRegistryID": "cas registry id",
                               "inChI": "inchi",
                               "SMILES": "smiles",
                               "hasCharge": "charge",
                               "hasMolecularWeight": "molecular weight",
                               "hasMolecularFormula": "molecular formula",
                               "hasGeometry": "geometry",
                               "spinMultiplicity": "spin multiplicity",
                               "hasAtom": "atoms",
                               "hasAtomicBond": "atomic bond",
                               "hasStandardEnthalpyOfFormation": "standard enthalpy of formation",
                               "pubChemCID": "CID",
                               }

    def create_score_model_dataset(self):
        question_set = []
        for idx, row in self.triples.iterrows():
            h, r, t = row
            if "Species_" in h and "EmpiricalFormula" not in t:
                q = self.property_names[r]
                h = self.entity2idx[h]
                t = self.entity2idx[t]
                r = self.relation2idx[r]
                question_set.append((q, h, t, r))

        df_question = pd.DataFrame(question_set)
        df_question.columns = ["question", "head", "tail", "rel"]
        df_question.to_csv(os.path.join(self.dataset_path, f'{self.dataset_name}/score_model_training.tsv'), sep='\t')


if __name__ == '__main__':
    my_reader = OntoChemistryReader()
    my_reader.create_score_model_dataset()
