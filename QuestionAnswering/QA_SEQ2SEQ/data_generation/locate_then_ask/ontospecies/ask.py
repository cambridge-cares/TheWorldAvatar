import random

import numpy as np
from constants.functions import StrOp

from constants.ontospecies import (
    ABSTRACT_IDENTIFIER_KEY,
    ABSTRACT_PROPERTY_KEY,
    KEY2LABELS,
    SPECIES_ABSTRACT_ATTRIBUTE_KEYS,
    OSIdentifierKey,
    OSPropertyKey,
    OSSpeciesAttrKey,
)
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph
from utils.numerical import normalize_1d


class OSAsker:
    def __init__(self):
        self.graph2sparql = Graph2Sparql()

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Species")

        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "What are " + verbalization

        return query_sparql, verbalization

    def ask_attribute(
        self, query_graph: QueryGraph, verbalization: str, attr_num: int = 1
    ):
        try:
            values_node = next(
                n
                for n in query_graph.get_objs("Species", "func")
                if query_graph.nodes[n].get("operator") is StrOp.VALUES
            )
            is_many_species = len(query_graph.nodes[values_node].get("operand")) > 1
        except StopIteration:
            is_many_species = False

        if is_many_species:
            query_graph.add_question_node("Species")

        will_sample_concrete_attribute = np.random.choice(
            [True, False],
            p=normalize_1d(
                [
                    len(OSPropertyKey)
                    + len(OSIdentifierKey)
                    + len([OSSpeciesAttrKey.CHEMCLASS, OSSpeciesAttrKey.USE]),
                    len(SPECIES_ABSTRACT_ATTRIBUTE_KEYS) * 2,
                ]
            ),
        )

        if will_sample_concrete_attribute:
            sampled_keys = [
                key for _, key in query_graph.nodes(data="key") if key is not None
            ]
            sampling_frame = (
                list(OSPropertyKey)
                + list(OSIdentifierKey)
                + [OSSpeciesAttrKey.CHEMCLASS, OSSpeciesAttrKey.USE]
            )
            sampling_frame = [x for x in sampling_frame if x not in sampled_keys]
            weights = [
                2 if isinstance(k, OSIdentifierKey) else 1 for k in sampling_frame
            ]
            keys = np.random.choice(
                sampling_frame,
                size=min(attr_num, len(sampling_frame)),
                replace=False,
                p=normalize_1d(weights),
            )
            # keys = random.sample(sampling_frame, k=min(attr_num, len(sampling_frame)))
            keys_label = []

            for key in keys:
                if isinstance(key, OSPropertyKey) or isinstance(key, OSIdentifierKey):
                    query_graph.add_question_node(key.value)
                    query_graph.add_triple("Species", "os:has" + key.value, key.value)
                elif key is OSSpeciesAttrKey.CHEMCLASS or key is OSSpeciesAttrKey.USE:
                    query_graph.add_question_node(key.value)
                    query_graph.add_triple(
                        "Species",
                        "os:has{key}/rdfs:label".format(key=key.value),
                        key.value,
                    )
                else:
                    raise Exception("Unexpected key: " + key)

                key_label = random.choice(KEY2LABELS[key])
                keys_label.append(key_label)

            query_sparql = self.graph2sparql.convert(query_graph)

            template = random.choice(
                [
                    "For {E}, {Q} its {K}",
                    "{Q} the {K} of {E}",
                ]
            )
            verbalization = template.format(
                Q=random.choice(["what is", "compare"])
                if is_many_species
                else "what is",
                E=verbalization,
                K=" and ".join(keys_label),
            )
        else:
            key = random.choice(SPECIES_ABSTRACT_ATTRIBUTE_KEYS)
            if key == ABSTRACT_PROPERTY_KEY:
                iri = "os:Property"
            elif key == ABSTRACT_IDENTIFIER_KEY:
                iri = "os:Identifier"
            else:
                raise ValueError("Unexpected abstract key: " + key)

            query_graph.add_question_node(key)
            query_graph.add_iri_node(iri=iri, prefixed=True, key=key)
            query_graph.add_triples(
                [("Species", "?has" + key, key), (key, "a/rdfs:subClassOf*", iri)]
            )

            key_label = random.choice(KEY2LABELS[key])

            query_sparql = self.graph2sparql.convert(query_graph)
            template = random.choice(["For {E}, {Q} its {K}", "{Q} the {K} of {E}"])
            verbalization = template.format(
                Q=random.choice(["what are", "compare"])
                if is_many_species
                else "what are",
                E=verbalization,
                K=key_label,
            )

        return query_sparql, verbalization
