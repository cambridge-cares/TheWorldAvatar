from transformers import PreTrainedTokenizer
from transformers.generation import GenerationMixin

from core.data_processing import T5_PREFIX_NL2SPARQL, postprocess_sparql, preprocess_nl
from core.data_processing.correct_sparql_prediction import SparqlPredictionCorrector
from core.data_processing.compact2verbose import SparqlCompact2VerboseConverter
from core.sparql import SparqlQuery


class SingleDomainTranslator:
    def __init__(
        self,
        model: GenerationMixin,
        tokenizer: PreTrainedTokenizer,
        max_new_tokens: int = 256,
    ):
        self.model = model
        self.tokenizer = tokenizer
        self.max_new_tokens = max_new_tokens
        self.pred_corrector = SparqlPredictionCorrector()
        self.compact2verbose = SparqlCompact2VerboseConverter()

    def _forward(self, text: str):
        encoded_input = self.tokenizer(text, return_tensors="pt").to(
            self.model.device
        )
        output_ids = self.model.generate(
            **encoded_input, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)

    def nl2sparql(self, question: str):
        question_encoded = preprocess_nl(question)

        pred_raw = self._forward(T5_PREFIX_NL2SPARQL + question_encoded)
        pred_decoded = postprocess_sparql(pred_raw)

        try:
            pred_decoded_parsed = SparqlQuery.fromstring(pred_decoded)

            try:
                pred_corrected = self.pred_corrector.correct(
                    sparql=pred_decoded_parsed, nlq=question
                )
                pred_corrected_str = str(pred_corrected)
            except Exception as e:
                print(e)
                pred_corrected_str = None

            try:
                pred_verbose = self.compact2verbose.convert(sparql_compact=pred_corrected)
                pred_verbose_str = str(pred_verbose)
            except Exception as e:
                print(e)
                pred_verbose_str = None

        except Exception as e:
            print(e)
            pred_corrected_str = None
            pred_verbose_str = None

        return dict(
            raw=pred_raw,
            decoded=pred_decoded,
            corrected=pred_corrected_str,
            verbose=pred_verbose_str,
        )
