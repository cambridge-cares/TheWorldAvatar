from services.translate.data_processing.postprocess import PostProcessor
from services.translate.sparql import SparqlQuery
from .compact2verbose import OKSparqlCompact2VerboseConverter

class OKPostProcessor(PostProcessor):
    def __init__(self, ontospecies_endpoint: str):
        self.compact2verbose = OKSparqlCompact2VerboseConverter(ontospecies_endpoint=ontospecies_endpoint)

    def postprocess(self, query: SparqlQuery, **kwargs):
        return self.compact2verbose.convert(query)