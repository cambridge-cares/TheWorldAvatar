import json
import os
import random

import numpy as np

from constants.fs import ROOTDIR
from locate_then_ask.ontokin.model import OKSpecies
from utils.numerical import normalize_1d


class OKSpeciesSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data/ontokin/Species.json")

    def __init__(self):
        with open(self.FILEPATH, "r") as f:
            data = json.load(f)

        self.species = data["Species"]

    def make(self):
        return OKSpecies(
            iri="placeholder",
            label=random.choice(self.species),
            mechanism_iris=[
                "placeholder"
                for _ in range(
                    np.random.choice([1, 2, 3, 4], p=normalize_1d([19, 10, 3, 23]))
                )
            ],
        )
