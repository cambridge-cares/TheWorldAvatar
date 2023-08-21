from transformers import AutoModelForSeq2SeqLM, AutoTokenizer

from marie.data_processing.qn_processing import preprocess_qn
from marie.model_utils import get_model_and_tokenizer
from marie.arguments_schema import ModelArguments


class TranslationModel:
    def __init__(
        self,
        model_args: ModelArguments,
        max_new_tokens: int = 256,
    ):
        self.model, self.tokenizer = get_model_and_tokenizer(model_args, is_trainable=False)
        self.max_new_tokens = max_new_tokens

    def __call__(self, question: str):
        question = preprocess_qn(question)

        input_ids = self.tokenizer(question, return_tensors="pt").input_ids.to(
            self.model.device
        )
        output_ids = self.model.generate(input_ids, max_new_tokens=self.max_new_tokens)
        query = self.tokenizer.decode(output_ids[0], skip_special_tokens=True)

        return query
