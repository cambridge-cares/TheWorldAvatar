import json
import os
import random

import numpy as np

from constants.fs import ROOTDIR
from locate_then_ask.ontokin.model import OKGasPhaseReaction
from utils.numerical import normalize_1d


class OKRxnSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data/ontokin/Reaction.json")

    def __init__(self):
        with open(self.FILEPATH, "r") as f:
            data = json.load(f)

        self.equations = data["Equation"]

    def make(self):
        return OKGasPhaseReaction(
            iri="placeholder",
            equations=random.sample(self.equations, k=4),
            reactant_iris=[
                "placeholder"
                for _ in range(np.random.choice([1, 2], p=normalize_1d([12, 30])))
            ],
            product_iris=[
                "placeholder"
                for _ in range(np.random.choice([1, 2, 3], p=normalize_1d([18, 23, 1])))
            ],
            mechanism_iris=[
                "placeholder"
                for _ in range(
                    np.random.choice([1, 2, 3, 4], p=normalize_1d([16, 20, 2, 14]))
                )
            ],
        )
