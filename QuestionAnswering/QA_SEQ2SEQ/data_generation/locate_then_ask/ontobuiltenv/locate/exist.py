import random

from constants.ontobuiltenv import OBE_ATTR_LABELS, OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.query_graph import QueryGraph


class OBEExistentialLocator:
    def locate(self, query_graph: QueryGraph, key: OBEAttrKey):
        query_graph.add_node(key.value, key=key)
        query_graph.add_triple("Property", "obe:has" + key.value, key.value)

        return random.choice(OBE_ATTR_LABELS[key]) + " exists"
