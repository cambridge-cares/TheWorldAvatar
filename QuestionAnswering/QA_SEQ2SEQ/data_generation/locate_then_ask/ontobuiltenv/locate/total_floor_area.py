from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.locate.measure import OBEOmMeasureLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph


class OBETotalFloorAreaLocator(OBEAttrLocator):
    def __init__(self):
        self.measure_locator = OBEOmMeasureLocator()
        
    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        assert entity.total_floor_area is not None
        
        return self.measure_locator.locate(
            query_graph,
            key=OBEAttrKey.TOTAL_FLOOR_AREA,
            measure=entity.total_floor_area,
        )
