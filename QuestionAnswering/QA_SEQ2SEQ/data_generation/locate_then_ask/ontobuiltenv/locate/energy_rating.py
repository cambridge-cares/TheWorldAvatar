from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph


class OBEEnergyRatingLocator(OBEAttrLocator):
    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        assert entity.energy_rating is not None

        energyrating_node = query_graph.make_literal_node(entity.energy_rating, key=OBEAttrKey.ENERGY_RATING)
        query_graph.add_triple("Property", "obe:hasEnergyRating", energyrating_node)

        verbn = "energy rating is [{label}]".format(label=entity.energy_rating)

        return verbn
