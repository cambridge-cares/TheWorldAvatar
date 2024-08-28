import random
from constants.functions import AggOp
from locate_then_ask.ask import ask_name

from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKSpeciesAsker:
    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        return ask_name(query_graph, verbalization, "Species")

    def ask_count(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Species", agg=AggOp.COUNT)

        if verbalization.startswith("the"):
            verbalization = verbalization[len("the") :].strip()

        query_sparql = Graph2Sparql.convert(query_graph)
        verbalization = "How many {x} are there".format(x=verbalization)

        return query_sparql, verbalization

    def ask_attribute_or_relation(
        self, query_graph: QueryGraph, verbalization: str, attr_num: int = 1
    ):
        if "Mechanism" not in query_graph.nodes() and random.getrandbits(1):
            ask_mechanism = True
            query_graph.add_question_node("Mechanism")
            query_graph.add_triple(
                "Species", "okin:belongsToPhase/^okin:hasGasPhase", "Mechanism"
            )
        else:
            ask_mechanism = False

        sampling_frame = ["ThermoModel", "TransportModel"]
        keys = random.sample(sampling_frame, k=min(len(sampling_frame), attr_num))

        for k in keys:
            query_graph.add_question_node(k)
            query_graph.add_triple("Species", "okin:has" + k, k)
            if "Mechanism" in query_graph.nodes():
                query_graph.add_triple(k, "okin:definedIn", "Mechanism")

        query_sparql = Graph2Sparql.convert(query_graph)

        attr_template = random.choice(
            [
                "For {E}, {V} its {K}",
                "{V} the {K} of {E}",
            ]
        )

        key2label = {
            "ThermoModel": "thermodynamic model",
            "TransportModel": "transport model",
        }
        verbalization = attr_template.format(
            V=random.choice(["what is", "compare"]) if ask_mechanism else "what is",
            E=verbalization,
            K=" and ".join([key2label[x] for x in keys if x in key2label]),
        )

        if ask_mechanism:
            verbalization += " across all the reaction mechanisms in which it appears"

        return query_sparql, verbalization
