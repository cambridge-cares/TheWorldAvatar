from services.translate.data_processing.postprocess import PostProcessor
from services.translate.sparql import SparqlQuery
from .compact2verbose import OBECompact2VerboseConverter

class OBEPostProcessor(PostProcessor):
    def __init__(self):
        self.compact2verbose = OBECompact2VerboseConverter()

    def postprocess(self, query: SparqlQuery, **kwargs):
        return self.compact2verbose.convert(query)
