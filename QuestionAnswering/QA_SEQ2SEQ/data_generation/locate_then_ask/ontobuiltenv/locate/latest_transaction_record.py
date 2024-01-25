from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.locate.exist import OBEExistentialLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph


class OBELatestTransactionRecordLocator(OBEAttrLocator):
    def __init__(self):
        self.existential_locator = OBEExistentialLocator()
        
    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        assert entity.latest_transaction_record is not None
        return self.existential_locator.locate(query_graph, key=OBEAttrKey.LATEST_TRANSACTION_RECORD)
