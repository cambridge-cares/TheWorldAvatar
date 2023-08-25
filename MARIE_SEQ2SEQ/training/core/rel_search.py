from abc import ABC, abstractmethod

import numpy as np
import Levenshtein
from transformers import BertTokenizer, BertModel
import torch


RELATIONS = [
    "hasAtomChiralCount",
    "hasAtomChiralDefCount",
    "hasAtomChiralUndefCount",
    "hashasBondChiralCount",
    "hasBondChiralDefCount",
    "hasBondChiralUndefCount",
    "hasCanonicalizedCompound",
    "hasCharge",
    "hasCompoundComplexity",
    "hasCovalentUnitCount",
    "hasExactMass",
    "hasHeavyAtomCount",
    "hasHydrogenBondAcceptorCount",
    "hasHydrogenBondDonorCount",
    "hasIsotopeAtomCount",
    "hasMolecularWeight",
    "hasMonoIsotopicWeight",
    "hasRotatableBondCount",
    "hasSubStructureKeysFingerprint",
    "hasTautomerCount",
    "hasXLogP3",
    "hasAutoignitionTemperature",
    "hasCaco2Permeability",
    "hasCollisionCrossSection",
    "hasHydrophobicity",
    "hasIonizationPotential",
    "hasIsoelectricPoint",
    "hasLogP",
    "hasLogS",
    "hasPolarSurfaceArea",
    "hasBoilingPoint",
    "hasDensity",
    "hasDissociationConstants",
    "hasEnthalpyOfSublimation",
    "hasFlashPoint",
    "hasStandardEnthalpyOfFormation",
    "hasHeatOfCombustion",
    "hasHeatOfVaporization",
    "hasHenrysLawConstant",
    "hasMeltingPoint",
    "hasOpticalRotation",
    "hasSolubility",
    "hasSurfaceTension",
    "hasVaporDensity",
    "hasVaporPressure",
    "hasViscosity",
]


class RelMetric(ABC):
    @abstractmethod
    def __call__(self, left: str, right: str):
        pass


class LevenshteinMetric(RelMetric):
    def __call__(self, left: str, right: str):
        return Levenshtein.distance(left, right)


def cosine_similarity(left, right):
    return np.dot(left, right) / (np.linalg.norm(left) * np.linalg.norm(right))


class BertMetric(RelMetric):
    def __init__(self, model: str = "bert-base-cased"):
        self.tokenizer = BertTokenizer.from_pretrained(model)
        self.model = BertModel.from_pretrained(model, output_hidden_states=True).eval()
        self.cache = dict()

    def _get_bert_embed(self, text: str):
        if text not in self.cache:
            inputs = self.tokenizer(text, return_tensors="pt")
            with torch.no_grad():
                outputs = self.model(**inputs)
            self.cache[text] = outputs.hidden_states[-1].squeeze().mean(axis=0)
        return self.cache[text]

    def __call__(self, left: str, right: str):
        left_embed = self._get_bert_embed(left)
        right_embed = self._get_bert_embed(right)

        return cosine_similarity(left_embed, right_embed)


class RelSearchModel:
    def __init__(self, model: int):
        """Instantiates a relation search model.

        Args:
            model: model for matching predicted relation with an actual KG relation. Can be `bert`, `levenshtein`
        """
        if model == "bert":
            metric = BertMetric()
        elif model == "levenshtein":
            metric = LevenshteinMetric()
        else:
            raise Exception(
                f"The argument `rel_search_model` is expected to be in (`bert`, `levenshtein`). {model} found."
            )
        self.metric = metric

    def get_nearest(self, predicted_rel: str):
        distances = [self.metric(predicted_rel, rel) for rel in RELATIONS]
        return RELATIONS[np.argmin(distances)]
