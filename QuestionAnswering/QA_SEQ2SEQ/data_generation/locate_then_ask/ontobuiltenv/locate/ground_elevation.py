from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.locate.measure import OBEOmMeasureLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph


class OBEGroundElevationLocator(OBEAttrLocator):
    def __init__(self):
        self.measure_locator = OBEOmMeasureLocator()

    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        if entity.ground_elevation is None:
            raise ValueError("The `ground_elevation` field of `entity` must not be None.")
        
        return self.measure_locator.locate(
            query_graph,
            key=OBEAttrKey.GROUND_ELEVATION,
            measure=entity.ground_elevation
        )