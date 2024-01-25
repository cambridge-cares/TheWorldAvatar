import os
from typing import Dict

from core.args_schema import ModelArguments
from core.data_processing.constants import T5_PREFIX_DOMAINCLS, T5_PREFIX_NL2SPARQL
from core.data_processing.nl import preprocess_nl
from core.data_processing.ontocompchem.postprocess import OCCPostProcessor
from core.data_processing.ontokin.postprocess import OKPostProcessor
from core.data_processing.ontospecies.postprocess import OSPostProcessor
from core.data_processing.postprocess import PostProcessor
from core.data_processing.sparql import postprocess_sparql
from core.sparql import SparqlQuery
from core.translate.model_wrapper import ModelWrapper


class MultiDomainTranslator:
    def __init__(
        self,
        model_args: ModelArguments,
        max_new_tokens: int = 256,
    ):
        self.model = ModelWrapper(model_args, max_new_tokens=max_new_tokens)
        self.domain2postprocessor: Dict[str, PostProcessor] = dict(
            ontospecies=OSPostProcessor(embedding_model_path=os.getenv("FEATURE_EXTRACTION_MODEL_PATH")),
            ontokin=OKPostProcessor(),
            ontocompchem=OCCPostProcessor(),
        )

    def nl2sparql(self, question: str):
        question_encoded = preprocess_nl(question)

        domain = self.model.forward(T5_PREFIX_DOMAINCLS + question_encoded)
        pred_raw = self.model.forward(T5_PREFIX_NL2SPARQL + question_encoded)
        pred_decoded = postprocess_sparql(pred_raw)

        try:
            pred_decoded_parsed = SparqlQuery.fromstring(pred_decoded)
            pred_verbose = self.domain2postprocessor[domain].postprocess(
                query=pred_decoded_parsed, nlq=question
            )
            pred_verbose_str = str(pred_verbose)
        except Exception as e:
            print(e)
            pred_verbose_str = None

        return dict(
            domain=domain,
            sparql=dict(
                raw=pred_raw,
                decoded=pred_decoded,
                verbose=pred_verbose_str,
            ),
        )
