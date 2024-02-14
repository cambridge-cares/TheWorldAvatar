from core.data_processing.ontocompchem.compact2verbose import OCCSparqlCompact2VerboseConverter
from core.data_processing.postprocess import PostProcessor
from core.sparql import SparqlQuery


class OCCPostProcessor(PostProcessor):
    def __init__(self):
        self.compact2verbose = OCCSparqlCompact2VerboseConverter()

    def postprocess(self, query: SparqlQuery, **kwargs):
        return self.compact2verbose.convert(query)
