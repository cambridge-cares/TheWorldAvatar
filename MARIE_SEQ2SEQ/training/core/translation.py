from abc import ABC, abstractmethod

import torch
from optimum.intel import OVModelForSeq2SeqLM, OVModelForCausalLM
from transformers import PreTrainedTokenizer

from core.data_processing.input_processing import preprocess_input, preprocess_input
from core.data_processing.output_processing import (
    postprocess_output,
    postprocess_output,
)
from core.model_utils import (
    get_hf_tokenizer,
    get_onmt_model_and_tokenizer,
    get_hf_model_and_tokenizer,
)
from core.arguments_schema import ModelArguments


class TranslationModel(ABC):
    def __init__(self, model_family: str):
        self.model_family = model_family

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


class _HfTranslationModelBase(TranslationModel):
    def __init__(
        self,
        model_family: str,
        model,
        tokenizer: PreTrainedTokenizer,
        max_new_tokens: int = 256,
    ):
        super().__init__(model_family)
        self.model = model
        self.tokenizer = tokenizer
        self.max_new_tokens = max_new_tokens

    def _translate(self, question: str):
        input_ids = self.tokenizer(question, return_tensors="pt").input_ids.to(
            self.model.device
        )
        output_ids = self.model.generate(
            input_ids=input_ids, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)


class HfTranslationModel(_HfTranslationModelBase):
    def __init__(
        self,
        model_args: ModelArguments,
        model_family: str,
        max_new_tokens: int = 256,
        do_torch_compile: bool = False,
    ):
        model, tokenizer = get_hf_model_and_tokenizer(
            model_args, is_trainable=False, model_family=model_family
        )
        if do_torch_compile:
            model = torch.compile(model)

        super().__init__(
            model_family=model_family,
            model=model,
            tokenizer=tokenizer,
            max_new_tokens=max_new_tokens,
        )


class OVHfTranslationModel(_HfTranslationModelBase):
    def __init__(
        self, model_args: ModelArguments, model_family: str, max_new_tokens: int = 256
    ):
        if model_family == "t5":
            model = OVModelForSeq2SeqLM.from_pretrained(model_args.model_path)
        elif model_family == "llama":
            model = OVModelForCausalLM.from_pretrained(model_args.model_path)
        else:
            raise ValueError("Unrecognised model family: " + model_family)

        model.reshape(1, 256)
        model.compile()

        tokenizer = get_hf_tokenizer(model_args.model_path, model_family=model_family)
        super().__init__(
            model_family=model_family,
            model=model,
            tokenizer=tokenizer,
            max_new_tokens=max_new_tokens,
        )

    def _translate(self, question: str):
        input_ids = self.tokenizer(
            question, return_tensors="pt", padding="max_length", max_length=128
        ).input_ids.to(self.model.device)
        output_ids = self.model.generate(
            input_ids=input_ids, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)


class ONmtTranslationModel(TranslationModel):
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
