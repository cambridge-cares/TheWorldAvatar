from core.data_processing.ontospecies.compact2verbose import OSSparqlCompact2VerboseConverter
from core.data_processing.ontospecies.correct_sparql_prediction import OSSparqlPredictionCorrector
from core.data_processing.postprocess import PostProcessor
from core.sparql import SparqlQuery


class OSPostProcessor(PostProcessor):
    def __init__(self):
        self.pred_corrector = OSSparqlPredictionCorrector()
        self.compact2verbose = OSSparqlCompact2VerboseConverter()

    def postprocess(self, query: SparqlQuery, nlq: str):
        pred_corrected = self.pred_corrector.correct(
            sparql=query, nlq=nlq
        )
        return self.compact2verbose.convert(sparql_compact=pred_corrected)
