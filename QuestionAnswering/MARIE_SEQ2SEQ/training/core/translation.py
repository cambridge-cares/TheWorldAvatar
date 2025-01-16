from abc import ABC, abstractmethod

import torch
from transformers import PreTrainedTokenizer
from core.data_processing.compact_query.compact_query_rep import CompactQueryRep
from core.data_processing.compact_query.correct_relations import RelationCorrector
from core.data_processing.compact_query.correct_spans import SpanCorrector
from core.data_processing.exceptions import InvalidCompactQueryError

from core.data_processing.input_processing import preprocess_input, preprocess_input
from core.data_processing.output_processing import (
    postprocess_output,
    postprocess_output,
)
from core.arguments_schema import ModelArguments
from core.model_utils.hf import get_hf_model_and_tokenizer
from core.model_utils.onmt import get_onmt_model_and_tokenizer
from core.model_utils.ort import get_ort_model_and_tokenizer
from core.model_utils.ov import get_ov_model_and_tokenizer


class TranslationModel(ABC):
    def __init__(self, model_family: str, do_correct: bool, embedding_model_path: str):
        self.model_family = model_family
        self.do_correct = do_correct
        self.span_corrector = SpanCorrector()
        self.relation_corrector = RelationCorrector(embedding_model_path)

    @abstractmethod
    def _translate(self, question: str):
        pass

    def nl2sparql(self, question: str):
        """Converts a natural language question to its corresponding SPARQL query.

        Returns:
            A dict with keys `prediction_raw` and `prediction_postprocessed`
        """
        question_encoded = preprocess_input(question, model_family=self.model_family)
        pred_raw = self._translate(question_encoded)
        pred_postprocessed = postprocess_output(
            pred_raw, model_family=self.model_family
        )
        try:
            query = CompactQueryRep.from_string(pred_postprocessed)
            if self.do_correct:
                query = self.span_corrector.correct(query, nlq=question)
                query = self.relation_corrector.correct(query)
                pred_corrected = query.to_string()
            else:
                pred_corrected = None
            pred_verbose = query.to_verbose()
        except Exception as e:
            if not isinstance(e, InvalidCompactQueryError):
                print("An unhandled error is encountered when parsing a compact query.")
                print(e)
            pred_corrected = None
            pred_verbose = None

        return dict(
            prediction_raw=pred_raw,
            prediction_postprocessed=pred_postprocessed,
            prediction_corrected=pred_corrected,
            prediction_verbose=pred_verbose,
        )


class _HfTranslationModelBase(TranslationModel):
    def __init__(
        self,
        model_family: str,
        do_correct: str,
        embedding_model_path: str,
        model,
        tokenizer: PreTrainedTokenizer,
        max_new_tokens: int = 256,
    ):
        super().__init__(
            model_family=model_family,
            do_correct=do_correct,
            embedding_model_path=embedding_model_path,
        )
        self.model = model
        self.tokenizer = tokenizer
        self.max_new_tokens = max_new_tokens

    def _translate(self, question: str):
        encoded_input = self.tokenizer(question, return_tensors="pt").to(
            self.model.device
        )
        output_ids = self.model.generate(
            **encoded_input, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)


class HfTranslationModel(_HfTranslationModelBase):
    def __init__(
        self,
        model_args: ModelArguments,
        do_correct: bool,
        embedding_model_path: str,
        max_new_tokens: int = 256,
        do_torch_compile: bool = False,
    ):
        model, tokenizer = get_hf_model_and_tokenizer(model_args, is_trainable=False)
        if do_torch_compile:
            model = torch.compile(model)

        super().__init__(
            model_family=model_args.model_family,
            do_correct=do_correct,
            embedding_model_path=embedding_model_path,
            model=model,
            tokenizer=tokenizer,
            max_new_tokens=max_new_tokens,
        )


class OVHfTranslationModel(_HfTranslationModelBase):
    def __init__(
        self,
        model_args: ModelArguments,
        do_correct: bool,
        embedding_model_path: str,
        max_input_tokens: int = 256,
        max_new_tokens: int = 256,
    ):
        self.max_input_tokens = 256
        model, tokenizer = get_ov_model_and_tokenizer(
            model_args, max_input_tokens=max_input_tokens
        )
        super().__init__(
            model_family=model_args.model_family,
            do_correct=do_correct,
            embedding_model_path=embedding_model_path,
            model=model,
            tokenizer=tokenizer,
            max_new_tokens=max_new_tokens,
        )

    def _translate(self, question: str):
        input_ids = self.tokenizer(
            question,
            return_tensors="pt",
            padding="max_length",
            max_length=self.max_input_tokens,
        ).input_ids.to(self.model.device)
        output_ids = self.model.generate(
            input_ids=input_ids, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)


class OrtHfTranslationModel(_HfTranslationModelBase):
    def __init__(
        self,
        model_args: ModelArguments,
        do_correct: bool,
        embedding_model_path: str,
        max_new_tokens: int = 256,
    ):
        model, tokenizer = get_ort_model_and_tokenizer(model_args)
        super().__init__(
            model_family=model_args.model_family,
            do_correct=do_correct,
            embedding_model_path=embedding_model_path,
            model=model,
            tokenizer=tokenizer,
            max_new_tokens=max_new_tokens,
        )


class ONmtTranslationModel(TranslationModel):
    def __init__(
        self,
        model_args: ModelArguments,
        do_correct: bool,
        embedding_model_path: str,
        max_new_tokens: int = 256,
    ):
        self.model, self.tokenizer = get_onmt_model_and_tokenizer(model_args)
        self.max_new_tokens = max_new_tokens
        super().__init__(
            model_family=model_args.model_family,
            do_correct=do_correct,
            embedding_model_path=embedding_model_path,
        )

    def _translate(self, question: str):
        input_tokens = self.tokenizer(question)
        output_tokens = self.model.translate_batch(
            [input_tokens],
            beam_size=1,
            min_decoding_length=0,
            max_decoding_length=self.max_new_tokens,
        )[0].hypotheses[0]
        return self.tokenizer.detokenize(output_tokens)
