from dataclasses import dataclass
import logging
from typing import Dict, Optional


from .data_processing.constants import T5_PREFIX_DOMAINCLS, T5_PREFIX_NL2SPARQL
from .data_processing.nl import preprocess_nl
from .data_processing.ontokin.postprocess import OKPostProcessor
from .data_processing.ontospecies.postprocess import OSPostProcessor
from .data_processing.ontocompchem.postprocess import OCCPostProcessor
from .data_processing.ontobuiltenv.postprocess import OBEPostProcessor
from .data_processing.singapore.postprocess import SgPostProcessor
from .data_processing.postprocess import IdentityPostProcessor, PostProcessor
from .data_processing.sparql import postprocess_sparql
from .sparql import SparqlQuery
from .triton_client.seq2seq_client import ISeq2SeqClient
from .triton_client.feature_extraction_client import IFeatureExtractionClient


@dataclass
class TranslateResultSparql:
    raw: str
    decoded: str
    verbose: Optional[str]


@dataclass
class TranslateResult:
    domain: str
    sparql: TranslateResultSparql


logger = logging.getLogger(__name__)


class Translator:
    def __init__(
        self,
        seq2seq_client: ISeq2SeqClient,
        feature_extraction_client: IFeatureExtractionClient,
    ):
        self.model = seq2seq_client
        self.domain2postprocessor: Dict[str, PostProcessor] = dict(
            ontospecies=OSPostProcessor(feature_extraction_client),
            ontokin=OKPostProcessor(),
            ontocompchem=OCCPostProcessor(),
            kingslynn=OBEPostProcessor(),
            singapore=SgPostProcessor(),
        )
        self.identity_postprocessor = IdentityPostProcessor()

    def nl2sparql(self, question: str, domain: Optional[str] = None):
        question_encoded = preprocess_nl(question)

        if domain is None:
            domain = self.model.forward(
                T5_PREFIX_DOMAINCLS + question_encoded, model="seq2seq_chemistry"
            )

        if domain in ["kingslynn", "singapore"]:
            model_name = domain
        else:
            model_name = "chemistry"
        pred_raw = self.model.forward(
            T5_PREFIX_NL2SPARQL + question_encoded, model="seq2seq_" + model_name
        )
        pred_decoded = postprocess_sparql(pred_raw)

        postprocessor = self.domain2postprocessor.get(
            domain, self.identity_postprocessor
        )

        try:
            pred_decoded_parsed = SparqlQuery.fromstring(pred_decoded)
            pred_verbose = postprocessor.postprocess(
                query=pred_decoded_parsed, nlq=question
            )
            pred_verbose_str = str(pred_verbose)
        except Exception as e:
            logger.error(e, exc_info=True)
            pred_verbose_str = None

        return TranslateResult(
            domain=domain,
            sparql=TranslateResultSparql(
                raw=pred_raw,
                decoded=pred_decoded,
                verbose=pred_verbose_str,
            ),
        )
