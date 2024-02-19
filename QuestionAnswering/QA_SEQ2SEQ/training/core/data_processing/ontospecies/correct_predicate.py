from sentence_transformers import SentenceTransformer, util

from core.data_processing.ontospecies.constants import (
    ABSTRACT_IDENTIFIER_KEY,
    ABSTRACT_PROPERTY_KEY,
    CHEMCLASS_KEY,
    IDENTIFIER_KEYS,
    PROPERTY_KEYS,
    USE_KEY,
)

special_words = ["InChIKey", "InChI", "XLogP3", "SMILES", "IUPAC", "Chebi", "CID", "ID"]


def tokenize(text: str):
    tokens = []

    ptr_prev = 0
    ptr = 1

    while ptr_prev < len(text):
        word = None
        for w in special_words:
            if text.startswith(w, ptr_prev):
                word = w
                break

        if word is not None:
            ptr = ptr_prev + len(word)
        else:
            while ptr < len(text) and not text[ptr].isupper():
                ptr += 1

        tokens.append(text[ptr_prev:ptr])
        ptr_prev = ptr
        ptr += 1

    return tokens


class OSPredicateCorrector:
    ABSTRACT_KEYS = [ABSTRACT_PROPERTY_KEY, ABSTRACT_IDENTIFIER_KEY]
    PROPERTY_IDENTIFIER_KEYS = PROPERTY_KEYS + IDENTIFIER_KEYS
    USE_CHEMCLASS_KEYS = [USE_KEY, CHEMCLASS_KEY]
    KEYS = ABSTRACT_KEYS + PROPERTY_IDENTIFIER_KEYS + USE_CHEMCLASS_KEYS

    VALID_PREDICATES = (
        ["?has" + x for x in ABSTRACT_KEYS]
        + ["os:has" + x for x in PROPERTY_IDENTIFIER_KEYS]
        + ["os:has{key}/os:value".format(key=x) for x in PROPERTY_IDENTIFIER_KEYS]
        + ["os:has" + x for x in USE_CHEMCLASS_KEYS]
        + ["os:has{key}/rdfs:label".format(key=x) for x in USE_CHEMCLASS_KEYS]
    )

    def __init__(self, model: str):
        self.model = SentenceTransformer(model)
        self.embed_matrix = self.model.encode(
            [" ".join(tokenize(x)) for x in self.KEYS], convert_to_tensor=True
        )

    def correct(self, predicate: str):
        if (
            not predicate.startswith("?has") and not predicate.startswith("os:has")
        ) or predicate in self.VALID_PREDICATES:
            return predicate

        key = predicate.split("/", maxsplit=1)[0].rsplit("has", maxsplit=1)[-1]

        embed_key = self.model.encode([" ".join(tokenize(key))], convert_to_tensor=True)
        cosine_scores = util.cos_sim(self.embed_matrix, embed_key).flatten()
        closest_idx = cosine_scores.argmax()
        closest_key = self.KEYS[closest_idx]

        if closest_key in self.ABSTRACT_KEYS:
            return "?has" + closest_key
        elif closest_key in self.PROPERTY_IDENTIFIER_KEYS:
            if "/" in predicate:
                return "os:has{key}/os:value".format(key=closest_key)
            else:
                return "os:has" + closest_key
        else:  # self.USE_CHEMCLASS_KEYS:
            if "/" in predicate:
                return "os:has{key}/rdfs:label".format(key=closest_key)
            else:
                return "os:has" + closest_key
