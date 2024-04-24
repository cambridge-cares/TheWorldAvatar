import json
import os
import random
from constants.fs import ROOTDIR
from locate_then_ask.ontocompchem.model import OCCSpecies


class OCCSpeciesSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data/ontocompchem/Species.json")

    def __init__(self):
        with open(self.FILEPATH, "r") as f:
            self.species = json.load(f)

    def make(self):
        return OCCSpecies(
            iri="placeholder",
            label=random.choice(self.species),
            molecular_computation_iris=["placeholder" for _ in range(3)],
        )
