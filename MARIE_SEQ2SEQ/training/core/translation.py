from abc import ABC, abstractmethod

import torch

from core.data_processing.input_processing import preprocess_input, preprocess_input
from core.data_processing.output_processing import (
    postprocess_output,
    postprocess_output,
)
from core.model_utils import (
    get_onmt_model_and_tokenizer,
    get_hf_model_and_tokenizer,
)
from core.arguments_schema import ModelArguments


class TranslationModel(ABC):
    @abstractmethod
    def _translate(self, question: str):
        pass

    def nl2sparql(self, question: str):
        """Converts a natural language question to its corresponding SPARQL query.

        Returns:
            A dict with keys `prediction_raw` and `prediction_postprocessed`
        """
        question = preprocess_input(question, model_family=self.model_family)
        pred_raw = self._translate(question)
        pred_postprocessed = postprocess_output(
            pred_raw, model_family=self.model_family
        )

        return dict(
            prediction_raw=pred_raw, prediction_postprocessed=pred_postprocessed
        )


class HfTranslationModel(TranslationModel):
    def __init__(
        self,
        model_args: ModelArguments,
        model_family: str,
        max_new_tokens: int = 256,
        do_torch_compile: bool = False
    ):
        self.model, self.tokenizer = get_hf_model_and_tokenizer(
            model_args, is_trainable=False, model_family=model_family
        )
        if do_torch_compile:
            self.model = torch.compile(self.model)
        self.model_family = model_family
        self.max_new_tokens = max_new_tokens

    def _translate(self, question: str):
        input_ids = self.tokenizer(question, return_tensors="pt").input_ids.to(
            self.model.device
        )
        output_ids = self.model.generate(
            input_ids=input_ids, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)


class CTranslate2TranslationModel(TranslationModel):
    def __init__(
        self,
        model_args: ModelArguments,
        model_family: str,
        max_new_tokens: int = 256,
    ):
        self.model, self.tokenizer = get_onmt_model_and_tokenizer(
            model_args, model_family=model_family
        )
        self.model_family = model_family
        self.max_new_tokens = max_new_tokens

    def _translate(self, question: str):
        input_tokens = self.tokenizer(question)
        output_tokens = self.model.translate_batch(
            [input_tokens],
            beam_size=1,
            min_decoding_length=0,
            max_decoding_length=self.max_new_tokens,
        )[0].hypotheses[0]
        return self.tokenizer.detokenize(output_tokens)
