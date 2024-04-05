import os

from .compact2verbose import SgCompact2VerboseConverter
from services.translate.data_processing.postprocess import PostProcessor
from services.translate.sparql import SparqlQuery


class SgPostProcessor(PostProcessor):
    def __init__(self):
        self.compact2verbose = SgCompact2VerboseConverter(ontop_endpoint=os.getenv("KG_ENDPOINT_SINGAPORE_ONTOP"))

    def postprocess(self, query: SparqlQuery, **kwargs):
        return self.compact2verbose.convert(query)
