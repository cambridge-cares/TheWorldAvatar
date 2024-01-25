import json
import os
import random
from constants.fs import ROOTDIR
from locate_then_ask.ontokin.model import OKMechanism


class OKMechanismSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data/ontokin/Mechanism.json")
    def __init__(self):
        with open(self.FILEPATH, "r") as f:
            data = json.load(f)

        self.dois = data["DOI"]

    def make(self):
        return OKMechanism(
            iri="placeholder",
            doi=random.choice(self.dois),
            species_iris=["placeholder" for _ in range(random.randint(20, 22))],
            reaction_iris=["placeholder" for _ in range(random.randint(19, 30))]
        )