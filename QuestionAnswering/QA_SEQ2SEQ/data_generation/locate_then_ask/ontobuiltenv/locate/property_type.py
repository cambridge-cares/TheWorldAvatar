import random

from constants.namespaces import OBE
from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph


class OBEPropertyTypeLocator(OBEAttrLocator):
    KEY2LABELS = {
        "House": ["house"],
        "Maisonette": ["maisonette"],
        "Bungalow": ["bungalow"],
        "ParkHome": ["park home", "mobile home"],
    }

    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        if entity.property_type is None:
            raise ValueError("The `property_type` of `entity` must not be None.")
        if not entity.property_type.startswith(OBE):
            raise ValueError(
                "`entity.property_type` must start with {expected}. Found: {actual}.".format(
                    expected=OBE, actual=entity.property_type
                )
            )

        clsname = entity.property_type[len(OBE) :]
        clsname_node = "obe:" + clsname
        query_graph.add_iri_node(
            clsname_node, prefixed=True, key=OBEAttrKey.PROPERTY_TYPE
        )
        query_graph.add_triple("Property", "obe:hasPropertyType/a", clsname_node)

        verbn = "property type is " + random.choice(self.KEY2LABELS[clsname])

        return verbn
