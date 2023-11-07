import copy
import random
from locate_then_ask.data_model import AskDatum
from locate_then_ask.ontokin.graph2sparql import OKGraph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKSpeciesAsker:
    def __init__(self):
        self.graph2sparql = OKGraph2Sparql()

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

    def ask_attribute_or_relation(self, query_graph: QueryGraph, verbalization: str, attr_num: int = 1):
        query_graph = copy.deepcopy(query_graph)

        attr_keys = ["ThermoModel", "TransportModel"]
        q_keys = attr_keys + ["Mechanism"]

        idxes = random.sample(list(range(len(q_keys))), k=min(attr_num, len(q_keys)))

        keys = []
        ask_mechanism = False
        for i in idxes:
            key = q_keys[i]
            if key == "Mechanism":
                ask_mechanism = True
                continue
            keys.append(key)
            query_graph.add_node(key, question_node=True)
            query_graph.add_edge("Species", key, label="okin:has" + key)

        if ask_mechanism:
            keys.append("Mechanism")
            query_graph.add_node("Mechanism", question_node=True)
            query_graph.add_edge("Species", "Mechanism", label="okin:belongsToPhase/okin:containedIn")

        query_sparql = self.graph2sparql.convert(query_graph)

        if "ThermoModel" in keys or "TransportModel" in keys:
            attr_template = random.choice(
                [
                    "For {E}, what is its {K}",
                    "What is the {K} of {E}",
                ]
            )

            key2label = {
                "ThermoModel": "thermodynamic model",
                "TransportModel": "transport model",
            }
            verbalization = attr_template.format(
                E=verbalization,
                K=" and ".join([key2label[x] for x in keys if x in key2label]),
            )

            if ask_mechanism:
                verbalization += " and the reaction mechanism in which it appears"
        else:
            attr_template = random.choice([
                "For {E}, what is the reaction mechanism in which it appears",
                "What is the reaction mechanism that {E} appears in"
            ])
            verbalization = attr_template.format(E=verbalization)

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
