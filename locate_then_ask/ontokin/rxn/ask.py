import copy
import random
from locate_then_ask.data_model import AskDatum
from locate_then_ask.ontokin.graph2sparql import OKGraph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKReactionAsker:
    def __init__(self):
        self.graph2sparql = OKGraph2Sparql()

    def _ask_mechanism(self, query_graph: QueryGraph, verbalization: str):
        if "Mechanism" in query_graph.nodes:
            return query_graph, verbalization

        query_graph = copy.deepcopy(query_graph)

        qnode = "Mechanism"
        query_graph.add_node(qnode, question_node=True)
        query_graph.add_edge(
            "Reaction", qnode, label="okin:belongsToPhase/okin:containedIn"
        )

        verbalization += " across all the mechanisms that it appears in"

        return query_graph, verbalization

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Reaction"]["question_node"] = True

        query_graph, verbalization = self._ask_mechanism(query_graph, verbalization)
        verbalization = "What is " + verbalization
        query_sparql = self.graph2sparql.convert(query_graph)

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )

    def ask_count(self, query_graph: QueryGraph, verbalization: str):
        assert "Mechanism" in query_graph.nodes()

        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Reaction"]["question_node"] = True
        query_graph.nodes["Reaction"]["ask_count"] = True

        if verbalization.startswith("the"):
            verbalization = verbalization[len("the"): ].strip()
            
        verbalization = "How many " + verbalization
        query_sparql = self.graph2sparql.convert(query_graph)

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )

    def ask_relation(self, query_graph: QueryGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)

        q = random.choice(["arrhenius_coeff", "falloff_coeff", "coeff"])
        if q == "arrhenius_coeff":
            K = "Arrhenius coefficients"
            qnode = "ArrheniusCoefficient"
            query_graph.add_node(qnode, question_node=True)
            query_graph.add_edge(
                "Reaction", qnode, label="okin:hasArrheniusCoefficient"
            )
        elif q == "falloff_coeff":
            K = "falloff model coefficients"
            qnode = "FallOffModelCoefficient"
            query_graph.add_node(qnode, question_node=True)
            query_graph.add_edge(
                "Reaction", qnode, label="okin:hasFallOffModelCoefficient"
            )
        else:
            K = "rate coefficients"
            qnode = "RateCoefficient"
            bclass_node = "okin:RateCoefficient"
            query_graph.add_nodes_from(
                [
                    (qnode, dict(question_node=True)),
                    (
                        bclass_node,
                        dict(
                            iri=bclass_node,
                            prefixed=True,
                            template_node=True,
                            label=bclass_node,
                        ),
                    ),
                ]
            )
            query_graph.add_edges_from(
                [
                    ("Reaction", qnode, dict(label="?hasRateCoefficient")),
                    (qnode, bclass_node, dict(label="a/rdfs:subClassOf*")),
                ]
            )

        template = random.choice(
            [
                "For {E}, what are its {K}",
                "What are the {K} of {E}",
            ]
        )
        verbalization = template.format(
            E=verbalization,
            K=K,
        )

        query_graph, verbalization = self._ask_mechanism(query_graph, verbalization)
        query_sparql = self.graph2sparql.convert(query_graph)

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
