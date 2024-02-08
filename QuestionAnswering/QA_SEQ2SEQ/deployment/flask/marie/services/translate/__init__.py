from typing import Dict

from .data_processing.constants import T5_PREFIX_DOMAINCLS, T5_PREFIX_NL2SPARQL
from .data_processing.nl import preprocess_nl
from .data_processing.ontokin.postprocess import OKPostProcessor
from .data_processing.ontospecies.postprocess import OSPostProcessor
from .data_processing.ontocompchem.postprocess import OCCPostProcessor
from .data_processing.postprocess import PostProcessor
from .data_processing.sparql import postprocess_sparql
from .sparql import SparqlQuery
from .triton_client.seq2seq_client import Seq2SeqClient


class MultiDomainTranslator:
    def __init__(self):
        self.model = Seq2SeqClient()
        self.domain2postprocessor: Dict[str, PostProcessor] = dict(
            ontospecies=OSPostProcessor(),
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
