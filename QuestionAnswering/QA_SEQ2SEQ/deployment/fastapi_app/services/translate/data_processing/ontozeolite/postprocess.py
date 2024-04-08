from services.translate.data_processing.map_predicate import PredicateMapper
from services.translate.data_processing.postprocess import PostProcessor
from services.translate.sparql import SparqlQuery
from .compact2verbose import OZCompact2VerboseConverter

class OZPostProcessor(PostProcessor):
    def __init__(self, ontospecies_endpoint: str):
        self.pred_mapper = PredicateMapper(mappings={"zeo:hasZeoliteTopology": "zeo:hasTopologicalProperties"})
        self.compact2verbose = OZCompact2VerboseConverter(ontospecies_endpoint)

    def postprocess(self, query: SparqlQuery, **kwargs):
        query = self.pred_mapper.map(query)
        return self.compact2verbose.convert(query)
