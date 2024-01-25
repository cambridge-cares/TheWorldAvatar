from core.data_processing.ontospecies.compact2verbose import OSSparqlCompact2VerboseConverter
from core.data_processing.ontospecies.correct_sparql_prediction import OSSparqlPredictionCorrector
from core.data_processing.postprocess import PostProcessor
from core.sparql import SparqlQuery


class OSPostProcessor(PostProcessor):
    def __init__(self, embedding_model_path: str):
        self.pred_corrector = OSSparqlPredictionCorrector(embedding_model_path)
        self.compact2verbose = OSSparqlCompact2VerboseConverter()

    def postprocess(self, query: SparqlQuery, **kwargs):
        pred_corrected = self.pred_corrector.correct(
            sparql=query, nlq=kwargs["nlq"]
        )
        return self.compact2verbose.convert(sparql_compact=pred_corrected)
