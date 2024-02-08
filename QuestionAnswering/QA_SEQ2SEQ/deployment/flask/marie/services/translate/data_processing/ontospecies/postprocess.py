from marie.services.translate.data_processing.postprocess import PostProcessor
from marie.services.translate.sparql import SparqlQuery
from .compact2verbose import OSSparqlCompact2VerboseConverter
from .correct_sparql_prediction import OSSparqlPredictionCorrector


class OSPostProcessor(PostProcessor):
    def __init__(self):
        self.pred_corrector = OSSparqlPredictionCorrector()
        self.compact2verbose = OSSparqlCompact2VerboseConverter()

    def postprocess(self, query: SparqlQuery, **kwargs):
        pred_corrected = self.pred_corrector.correct(
            sparql=query, nlq=kwargs["nlq"]
        )
        return self.compact2verbose.convert(sparql_compact=pred_corrected)
