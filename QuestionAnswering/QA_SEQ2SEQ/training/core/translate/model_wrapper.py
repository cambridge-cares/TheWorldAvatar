from core.args_schema import ModelArguments
from core.model_utils import get_hf_model_and_tokenizer, get_ort_model_and_tokenizer


class ModelWrapper:
    def __init__(
        self,
        model_args: ModelArguments,
        max_new_tokens: int = 256,
    ):
        if model_args.model_format == "hf":
            model, tokenizer = get_hf_model_and_tokenizer(model_args)
        elif model_args.model_format == "ort":
            model, tokenizer = get_ort_model_and_tokenizer(model_args)
            
        self.model = model
        self.tokenizer = tokenizer
        self.max_new_tokens = max_new_tokens

    def forward(self, text: str):
        encoded_input = self.tokenizer(text, return_tensors="pt").to(self.model.device)
        output_ids = self.model.generate(
            **encoded_input, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)
