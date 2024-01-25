from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.locate.measure import OBEOmMeasureLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph


class OBEMarketValueLocator(OBEAttrLocator):
    def __init__(self):
        self.measure_locator = OBEOmMeasureLocator()

    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        assert entity.market_value is not None

        return self.measure_locator.locate(
            query_graph,
            key=OBEAttrKey.MARKET_VALUE,
            measure=entity.market_value
        )
    