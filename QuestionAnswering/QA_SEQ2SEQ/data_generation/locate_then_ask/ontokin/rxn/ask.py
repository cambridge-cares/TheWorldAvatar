import random
from constants.functions import AggOp

from locate_then_ask.ask import ask_name
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKReactionAsker:
    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        return ask_name(query_graph, verbalization, "Reaction")

    def ask_count(self, query_graph: QueryGraph, verbalization: str):
        assert "Mechanism" in query_graph.nodes()

        query_graph.add_question_node("Reaction", agg=AggOp.COUNT)

        if verbalization.startswith("the"):
            verbalization = verbalization[len("the") :].strip()

        verbalization = "How many {x} are there".format(x=verbalization)
        query_sparql = Graph2Sparql.convert(query_graph)

        return query_sparql, verbalization

    def ask_relation(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("KineticModel")
        query_graph.add_triple("Reaction", "okin:hasKineticModel", "KineticModel")

        template = random.choice(
            [
                "For {E}, what is its {ATTR}",
                "What is the {ATTR} of {E}",
            ]
        )

        if "Mechanism" not in query_graph.nodes() and random.getrandbits(1):
            query_graph.add_question_node("Mechanism")
            query_graph.add_triple("Reaction", "^okin:hasReaction", "Mechanism")
            template += " across all mechanisms it {V}".format(
                V=random.choice(
                    ["is involved in", "is part of", "is featured in", "appears in"]
                )
            )

        if "Mechanism" in query_graph.nodes():
            query_graph.add_triple("KineticModel", "okin:definedIn", "Mechanism")

        verbalization = template.format(
            E=verbalization,
            ATTR=random.choice(
                [
                    "kinetic model",
                    "kinetic model parameters",
                    "rate constants",
                    "rate constant parameters",
                ]
            ),
        )

        query_sparql = Graph2Sparql.convert(query_graph)

        return query_sparql, verbalization
