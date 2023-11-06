import copy
import random
from locate_then_ask.data_model import AskDatum
from locate_then_ask.ontokin.graph2sparql import OKGraph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKReactionAsker:
    def __init__(self):
        self.graph2sparql = OKGraph2Sparql()

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Reaction"]["question_node"] = True

        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "What are " + verbalization

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
            query_graph.add_edge("Reaction", qnode, label="okin:hasFallOffModelCoefficient")
        else:
            K = "rate coefficients"
            qnode = "RateCoefficient"
            bclass_node = "okin:RateCoefficient"
            query_graph.add_nodes_from(
                [
                    (qnode, dict(question_node=True)),
                    (bclass_node, dict(iri=bclass_node, prefixed=True, template_node=True, label=bclass_node)),
                ]
            )
            query_graph.add_edges_from(
                [
                    ("Reaction", qnode, dict(label="?hasRateCoefficient")),
                    (qnode, bclass_node, dict(label="rdfs:subClassOf*")),
                ]
            )

        query_sparql = self.graph2sparql.convert(query_graph)
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

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
