from services.translate.triton_client.feature_extraction_client import (
    IFeatureExtractionClient,
)
from services.translate.data_processing.postprocess import PostProcessor
from services.translate.sparql import SparqlQuery
from .compact2verbose import OSSparqlCompact2VerboseConverter
from .correct_sparql_prediction import OSSparqlPredictionCorrector


class OSPostProcessor(PostProcessor):
    def __init__(self, feature_extraction_client: IFeatureExtractionClient):
        self.pred_corrector = OSSparqlPredictionCorrector(feature_extraction_client)
        self.compact2verbose = OSSparqlCompact2VerboseConverter()

    def postprocess(self, query: SparqlQuery, **kwargs):
        pred_corrected = self.pred_corrector.correct(sparql=query, nlq=kwargs["nlq"])
        return self.compact2verbose.convert(sparql_compact=pred_corrected)
