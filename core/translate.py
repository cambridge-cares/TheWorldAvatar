from transformers import PreTrainedTokenizer
from transformers.generation import GenerationMixin

from core.args_schema import ModelArguments
from core.data_processing import T5_PREFIX_CLSDOMAIN, T5_PREFIX_NL2SPARQL, postprocess_sparql, preprocess_nl
from core.data_processing.correct_sparql_prediction import SparqlPredictionCorrector
from core.data_processing.compact2verbose import SparqlCompact2VerboseConverter
from core.model_utils import get_hf_model_and_tokenizer, get_ort_model_and_tokenizer
from core.sparql import SparqlQuery


class Translator:
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
    
    def _translate_to_sparql(self, question: str):
        return self._forward(T5_PREFIX_NL2SPARQL + question)
    
    def _classify_domain(self, question: str):
        return self._forward(T5_PREFIX_CLSDOMAIN + question)

    def nl2sparql(self, question: str):
        """Converts a natural language question to its corresponding SPARQL query.

        Returns:
            A dict with keys `prediction_raw` and `prediction_postprocessed`
        """
        question_encoded = preprocess_nl(question)

        domain = self._classify_domain(question_encoded)

        pred_raw = self._translate_to_sparql(question_encoded)
        pred_decoded = postprocess_sparql(pred_raw)
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

        return dict(
            domain=domain,
            raw=pred_raw,
            decoded=pred_decoded,
            corrected=pred_corrected_str,
            verbose=pred_verbose_str,
        )


class HfTranslator(Translator):
    def __init__(
        self,
        model_args: ModelArguments,
        max_new_tokens: int = 256,
    ):
        model, tokenizer = get_hf_model_and_tokenizer(model_args)
        super().__init__(
            model=model,
            tokenizer=tokenizer,
            max_new_tokens=max_new_tokens,
        )


class OrtHfTranslator(Translator):
    def __init__(self, model_args: ModelArguments, max_new_tokens: int = 256):
        model, tokenizer = get_ort_model_and_tokenizer(model_args)
        super().__init__(
            model=model,
            tokenizer=tokenizer,
            max_new_tokens=max_new_tokens,
        )
