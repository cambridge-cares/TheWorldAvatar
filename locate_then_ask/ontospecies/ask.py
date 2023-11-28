import copy
import random

from constants.ontospecies import (
    ABSTRACT_IDENTIFIER_KEY,
    ABSTRACT_PROPERTY_KEY,
    CHEMCLASS_KEY,
    IDENTIFIER_KEYS,
    KEY2LABELS,
    PROPERTY_KEYS,
    SPECIES_ABSTRACT_ATTRIBUTE_KEYS,
    SPECIES_ATTRIBUTE_KEYS,
    USE_KEY,
)
from locate_then_ask.data_model import AskDatum
from locate_then_ask.ontospecies.graph2sparql import OSGraph2Sparql
from locate_then_ask.query_graph import QueryGraph, get_preds


class OSAsker:
    def __init__(self):
        self.graph2sparql = OSGraph2Sparql()

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Species"]["question_node"] = True

        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "What are " + verbalization

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )

    def ask_attribute(
        self, query_graph: QueryGraph, verbalization: str, attr_num: int = 1
    ):
        query_graph = copy.deepcopy(query_graph)

        will_sample_concrete_attribute = random.sample(
            population=[True, False],
            counts=[len(SPECIES_ATTRIBUTE_KEYS), len(SPECIES_ABSTRACT_ATTRIBUTE_KEYS)],
            k=1,
        )[0]

        if will_sample_concrete_attribute:
            sampled_keys = [
                x[len("os:has") :]
                for x in get_preds(query_graph, "Species")
                if x.startswith("os:has")
            ]
            key_sampling_frame = [
                x for x in SPECIES_ATTRIBUTE_KEYS if x not in sampled_keys
            ]
            keys = random.sample(
                key_sampling_frame, k=min(attr_num, len(key_sampling_frame))
            )
            keys_label = []

            for key in keys:
                if key in PROPERTY_KEYS + IDENTIFIER_KEYS:
                    obj = key
                    predicate = "os:has{Name}".format(Name=key)
                elif key in [USE_KEY, CHEMCLASS_KEY]:
                    obj = key + "Label"
                    predicate = "os:has{Name}/rdfs:label".format(Name=key)
                else:
                    raise ValueError("Unexpected key: " + key)

                query_graph.add_node(obj, question_node=True)
                query_graph.add_edge("Species", obj, label=predicate)

                key_label = random.choice(KEY2LABELS[key])
                keys_label.append(key_label)

            query_sparql = self.graph2sparql.convert(query_graph)

            species_num = (
                1
                if not isinstance(query_graph.nodes["Species"]["label"], list)
                else len(query_graph.nodes["Species"]["label"])
            )

            template = random.choice(
                [
                    "For {E}, what is {possessive_adj} {K}",
                    "What is the {K} of {E}",
                ]
            )
            verbalization = template.format(
                E=verbalization,
                possessive_adj="their" if species_num > 1 else "its",
                K=" and ".join(keys_label),
            )
        else:
            key = random.choice(SPECIES_ABSTRACT_ATTRIBUTE_KEYS)
            if key == ABSTRACT_PROPERTY_KEY:
                abstract_key = "os:Property"
            elif key == ABSTRACT_IDENTIFIER_KEY:
                abstract_key = "os:Identifier"
            else:
                raise ValueError("Unexpected abstract key: " + key)

            query_graph.add_nodes_from(
                [
                    (key, dict(question_node=True)),
                    (
                        abstract_key,
                        dict(iri=abstract_key, template_node=True, prefixed=True),
                    ),
                ]
            )
            query_graph.add_edge("Species", key, label="?has{key}".format(key=key))
            query_graph.add_edge(key, abstract_key, label="a/rdfs:subClassOf")

            key_label = random.choice(KEY2LABELS[key])

            query_sparql = self.graph2sparql.convert(query_graph)
            template = random.choice([
                "For {E}, what are its {K}",
                "What are the {K} of {E}"
            ])
            verbalization = template.format(
                E=verbalization, K=key_label
            )

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
