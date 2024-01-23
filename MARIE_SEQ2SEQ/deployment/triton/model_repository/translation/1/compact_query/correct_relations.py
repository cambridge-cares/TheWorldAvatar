from typing import Dict, List, Tuple

from sentence_transformers import SentenceTransformer, util

from .constants import (
    IDENTIFIER_NAMES,
    PROPERTY_NAMES,
)
from .compact_query_rep import CompactQueryRep

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


class RelationCorrector:
    ABSTRACT_RELATION_NAMES = ["PropertyName", "IdentifierName"]
    USE_AND_CHEMICALCLASS_NAMES = ["Use", "ChemicalClass"]
    RELATION_NAMES = (
        PROPERTY_NAMES
        + IDENTIFIER_NAMES
        + USE_AND_CHEMICALCLASS_NAMES
        + ABSTRACT_RELATION_NAMES
    )
    VALID_RELATIONS = (
        [
            "os:has" + x
            for x in PROPERTY_NAMES + IDENTIFIER_NAMES + USE_AND_CHEMICALCLASS_NAMES
        ]
        + ["?has" + x for x in ABSTRACT_RELATION_NAMES]
        + ["?hasIdentifier"]
    )

    def __init__(self, model: str, threshold: float = 0):
        self.model = SentenceTransformer(model)
        self.embed_matrix = self.model.encode(
            [" ".join(tokenize(x)) for x in self.RELATION_NAMES], convert_to_tensor=True
        )
        self.threshold = threshold

    def _correct_relation(self, clause: str) -> Tuple[str, Dict[str, str]]:
        try:
            head, relation, tail = clause[: -len(".")].strip().split()
        except:
            return clause, dict()

        if relation in self.VALID_RELATIONS:
            return clause, dict()

        if not relation.startswith("os:has"):
            if not relation.startswith("?has"):
                return clause, dict()
            else:
                name = relation[len("?has") :]
        else:
            name = relation[len("os:has") :]

        embed_name = self.model.encode(
            [" ".join(tokenize(name))], convert_to_tensor=True
        )
        consine_scores = util.cos_sim(self.embed_matrix, embed_name).flatten()
        closest_idx = consine_scores.argmax()
        if consine_scores[closest_idx] < self.threshold:
            return clause, dict()

        closest_name = self.RELATION_NAMES[closest_idx]
        if closest_idx < len(self.RELATION_NAMES) - len(self.ABSTRACT_RELATION_NAMES):
            relation_corrected = "os:has" + closest_name
        else:
            relation_corrected = "?has" + closest_name

        if name in tail:
            # tail_corrected = tail.replace(name, closest_name) # correction to be done in CompactQueryRep::correct_relations
            name_mappings = {name: closest_name}
        else:
            tail_corrected = tail
            name_mappings = dict()

        return f"{head} {relation_corrected} {tail} .", name_mappings

    def _correct_relations(self, where_clauses: List[str]):
        where_clauses = list(where_clauses)

        triples: List[str] = []
        triple_idxes: List[int] = []

        for i, clause in enumerate(where_clauses):
            if clause.endswith("."):
                triples.append(clause)
                triple_idxes.append(i)

        if len(triples) == 0:
            return where_clauses

        name_mappings_lst: List[Dict[str, str]] = []
        for idx, triple in zip(triple_idxes, triples):
            where_clauses[idx], name_mappings = self._correct_relation(triple)
            name_mappings_lst.append(name_mappings)

        name_mappings = {
            k: v for mappings in name_mappings_lst for k, v in mappings.items()
        }

        return where_clauses, name_mappings

    def correct(self, query: CompactQueryRep):
        where_clauses, name_mappings = self._correct_relations(
            where_clauses=query.where_clauses
        )

        select_variables = query.select_variables.split()
        select_variables_corrected = []
        for x in select_variables:
            for k, v in name_mappings.items():
                x = x.replace(k, v)
            select_variables_corrected.append(x)

        where_clauses_corrected = []
        for clause in where_clauses:
            if any(clause.startswith(x) for x in ["VALUES", "FILTER"]):
                for k, v in name_mappings.items():
                    clause = clause.replace(k, v)
            elif clause.endswith("."):
                try:
                    head, rel, tail = clause[: -len(".")].strip().split()
                except:
                    pass
                for k, v in name_mappings.items():
                    head = head.replace(k, v)
                for k, v in name_mappings.items():
                    tail = tail.replace(k, v)
                clause = f"{head} {rel} {tail} ."
            else:
                raise InvalidCompactQueryError("Unexpected where clause: " + clause)
            where_clauses_corrected.append(clause)

        return CompactQueryRep(
            " ".join(select_variables_corrected), where_clauses_corrected
        )
