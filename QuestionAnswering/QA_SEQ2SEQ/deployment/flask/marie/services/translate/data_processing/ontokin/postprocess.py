from .compact2verbose import OKSparqlCompact2VerboseConverter
from marie.services.translate.data_processing.postprocess import PostProcessor
from marie.services.translate.sparql import SparqlQuery


class OKPostProcessor(PostProcessor):
    def __init__(self):
        self.compact2verbose = OKSparqlCompact2VerboseConverter()

    def postprocess(self, query: SparqlQuery, **kwargs):
        return self.compact2verbose.convert(query)