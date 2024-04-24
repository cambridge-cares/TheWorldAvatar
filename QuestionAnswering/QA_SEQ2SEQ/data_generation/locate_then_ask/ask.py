import random

from constants.nl import ENTITY_QN_WORDS
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


def ask_name(query_graph: QueryGraph, verbalization: str, question_node: str):
    query_graph.add_question_node(question_node)

    template = "{qnword} {entity}"
    verbalization = template.format(
        qnword=random.choice(ENTITY_QN_WORDS), entity=verbalization
    )
    query_sparql = Graph2Sparql.convert(query_graph)

    return query_sparql, verbalization