from core.data_processing.ontokin.compact2verbose import OKSparqlCompact2VerboseConverter
from core.data_processing.postprocess import PostProcessor
from core.sparql import SparqlQuery


class OKPostProcessor(PostProcessor):
    def __init__(self):
        self.compact2verbose = OKSparqlCompact2VerboseConverter()

    def postprocess(self, query: SparqlQuery, **kwargs):
        return self.compact2verbose.convert(query)