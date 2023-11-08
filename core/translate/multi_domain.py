from core.args_schema import ModelArguments
from core.data_processing import T5_PREFIX_DOMAINCLS, preprocess_nl
from core.model_utils import get_hf_model_and_tokenizer, get_ort_model_and_tokenizer
from core.translate.single_domain import SingleDomainTranslator


class MultiDomainTranslator(SingleDomainTranslator):
    def classify_domain(self, question: str):
        question_encoded = preprocess_nl(question)
        return self._forward(T5_PREFIX_DOMAINCLS + question_encoded)


class HfMultiDomainTranslator(MultiDomainTranslator):
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


class OrtHfMultiDomainTranslator(MultiDomainTranslator):
    def __init__(self, model_args: ModelArguments, max_new_tokens: int = 256):
        model, tokenizer = get_ort_model_and_tokenizer(model_args)
        super().__init__(
            model=model,
            tokenizer=tokenizer,
            max_new_tokens=max_new_tokens,
        )
