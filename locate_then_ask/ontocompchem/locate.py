from locate_then_ask.ontocompchem.entity_store import OCCEntityStore
from locate_then_ask.query_graph import QueryGraph


class OCCLocator:
    def __init__(self):
        self.store = OCCEntityStore()

    def _locate(self, query_graph: QueryGraph):
        pass
    
    def locate(self, entity_iri: str):
        pass