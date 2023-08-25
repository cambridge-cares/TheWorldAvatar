from abc import ABC, abstractmethod
from core.data_processing.qn_processing import preprocess_qn, preprocess_qn
from core.data_processing.query_processing import postprocess_query, postprocess_query
from core.model_utils import (
    get_ctranslate2_model_and_tokenizer,
    get_hf_model_and_tokenizer,
)
from core.arguments_schema import ModelArguments


class TranslationModel(ABC):
    @abstractmethod
    def nl2sparql(self, question: str):
        """Converts a natural language question to its corresponding SPARQL query.

        Returns:
            A dict with keys `prediction_raw` and `prediction_postprocessed`
        """
        pass


class HfTranslationModel(TranslationModel):
    def __init__(
        self,
        model_args: ModelArguments,
        model_family: str,
        max_new_tokens: int = 256,
    ):
        self.model, self.tokenizer = get_hf_model_and_tokenizer(
            model_args, is_trainable=False, model_family=model_family
        )
        self.model_family = model_family
        self.max_new_tokens = max_new_tokens

    def nl2sparql(self, question: str):
        question = preprocess_qn(question, model_family=self.model_family)

        input_ids = self.tokenizer(question, return_tensors="pt").input_ids.to(
            self.model.device
        )
        output_ids = self.model.generate(
            input_ids=input_ids, max_new_tokens=self.max_new_tokens
        )

        pred_raw = self.tokenizer.decode(output_ids[0], skip_special_tokens=True)
        pred_postprocessed = postprocess_query(pred_raw, model_family=self.model_family)

        return dict(
            prediction_raw=pred_raw, prediction_postprocessed=pred_postprocessed
        )


class CTranslate2TranslationModel(TranslationModel):
    def __init__(
        self,
        model_args: ModelArguments,
        model_family: str,
    ):
        self.model, self.tokenizer = get_ctranslate2_model_and_tokenizer(
            model_args, is_trainable=False, model_family=model_family
        )
        self.model_family = model_family

    def nl2sparql(self, question: str):
        question = preprocess_qn(question, model_family=self.model_family)

        input_tokens = self.tokenizer.convert_ids_to_tokens(
            self.tokenizer.encode(question)
        )
        output_tokens = self.model.translate_batch([input_tokens])[0].hypotheses[0]

        pred_raw = self.tokenizer.decode(
            self.tokenizer.convert_tokens_to_ids(output_tokens)
        )
        pred_postprocessed = postprocess_query(pred_raw, model_family=self.model_family)

        return dict(
            prediction_raw=pred_raw, prediction_postprocessed=pred_postprocessed
        )
