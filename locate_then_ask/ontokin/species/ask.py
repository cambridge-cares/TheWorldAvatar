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
        verbalization = "What is " + verbalization

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )

    def ask_count(self, query_graph: QueryGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Species"]["question_node"] = True
        query_graph.nodes["Species"]["ask_count"] = True

        if verbalization.startswith("the"):
            verbalization = verbalization[len("the"): ].strip()
            
        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "How many " + verbalization

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )

    def ask_attribute_or_relation(self, query_graph: QueryGraph, verbalization: str, attr_num: int = 1):
        query_graph = copy.deepcopy(query_graph)

        sampling_frame = ["ThermoModel", "TransportModel"]
        keys = random.sample(sampling_frame, k=min(len(sampling_frame), attr_num))

        for k in keys:
            query_graph.add_node(k, question_node=True)
            query_graph.add_edge("Species", k, label="okin:has" + k)

        if "Mechanism" not in query_graph.nodes():
            ask_mechanism = True
            query_graph.add_node("Mechanism", question_node=True)
            query_graph.add_edge("Species", "Mechanism", label="okin:belongsToPhase/^okin:hasGasPhase")
        else:
            ask_mechanism = False

        for n in sampling_frame:
            if n in query_graph.nodes():
                query_graph.add_edge(n, "Mechanism", label="okin:definedIn")

        query_sparql = self.graph2sparql.convert(query_graph)

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
            verbalization += " across all the reaction mechanisms in which it appears"

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
