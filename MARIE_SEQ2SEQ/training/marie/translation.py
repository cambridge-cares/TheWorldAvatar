from transformers import AutoModelForSeq2SeqLM, AutoTokenizer

from marie.data_processing.qn_processing import preprocess_qn


class TranslationModel:
    def __init__(
        self,
        model_path: str = "google/flan-t5-base",
        device="cuda",
        max_new_tokens=512,
    ):
        self.model = AutoModelForSeq2SeqLM.from_pretrained(
            model_path, device_map=device
        )
        self.tokenizer = AutoTokenizer.from_pretrained(model_path)
        self.device = device
        self.max_new_tokens = max_new_tokens

    def __call__(self, question: str):
        question = preprocess_qn(question)

        input_ids = self.tokenizer(question, return_tensors="pt").input_ids.to(
            self.device
        )
        output_ids = self.model.generate(input_ids, max_new_tokens=self.max_new_tokens)
        query = self.tokenizer.decode(output_ids[0], skip_special_tokens=True)

        return query
