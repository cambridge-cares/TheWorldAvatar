import json
import os
import random
from constants.fs import ROOTDIR
from locate_then_ask.ontocompchem.model import OCCMolecularComputation


class OCCMolecularComputationSynthesizer:
    DIRPATH = os.path.join(ROOTDIR, "data/ontocompchem")

    def __init__(self):
        with open(os.path.join(self.DIRPATH, "LevelOfTheory.json"), "r") as f:
            self.lots = json.load(f)

        with open(os.path.join(self.DIRPATH, "BasisSet.json"), "r") as f:
            self.bss = json.load(f)

    def make(self):
        return OCCMolecularComputation(
            iri="placeholder",
            species_iri="placeholder",
            level_of_theory=random.choice(self.lots),
            basis_set=random.choice(self.bss),
        )
