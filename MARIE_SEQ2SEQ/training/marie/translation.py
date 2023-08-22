from transformers import AutoModelForSeq2SeqLM, AutoTokenizer

from marie.data_processing.qn_processing import t5_preprocess_qn
from marie.model_utils import get_model_and_tokenizer, get_model_family_from_model_name
from marie.arguments_schema import ModelArguments
from training.marie.data_processing.query_processing import postprocess_query


class TranslationModel:
    def __init__(
        self,
        model_args: ModelArguments,
        max_new_tokens: int = 256,
    ):
        self.model, self.tokenizer = get_model_and_tokenizer(
            model_args, is_trainable=False
        )
        self.model_family = (
            model_args.model_family
            if model_args.model_family is not None
            else get_model_family_from_model_name(model_args.model_path)
        )
        self.max_new_tokens = max_new_tokens

    def nl2sparql(self, question: str):
        """Converts a natural language question to its corresponding SPARQL query.
        
        Returns:
            A dict with keys `prediction_raw` and `prediction_postprocessed`
        """
        question = t5_preprocess_qn(question, model_family=self.model_family)

        input_ids = self.tokenizer(question, return_tensors="pt").input_ids.to(
            self.model.device
        )
        output_ids = self.model.generate(
            input_ids=input_ids, max_new_tokens=self.max_new_tokens
        )

        pred_raw = self.tokenizer.decode(output_ids[0], skip_special_tokens=True)
        pred_postprocessed = postprocess_query(pred_raw, model_family=self.model_family)

        return dict(
            prediction_raw=pred_raw, predicition_postprocessed=pred_postprocessed
        )
