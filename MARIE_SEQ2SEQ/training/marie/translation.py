from transformers import AutoModelForSeq2SeqLM, AutoTokenizer

from marie.data_processing.qn_processing import preprocess_qn
from marie.data_processing.query_processing import postprocess_query

# from marie.rel_search import RelSearchModel


class TranslationModel:
    def __init__(
        self,
        model_path: str = "google/flan-t5-base",
        device="cuda",
        max_new_tokens=512,
        # rel_search_model: str = "levenshtein",
    ):
        self.model = AutoModelForSeq2SeqLM.from_pretrained(
            model_path, device_map=device
        )
        self.tokenizer = AutoTokenizer.from_pretrained(model_path)
        self.device = device
        self.max_new_tokens = max_new_tokens
        # self.rel_search_model = RelSearchModel(model=rel_search_model)

    # def correct_rel(self, query: str):
    #     idx = advance_idx_to_kw(query, "WHERE", 0)
    #     if not query.startswith("WHERE", idx):
    #         return query

    #     idx = advance_idx_to_kw(query, "{", idx + len("WHERE"))
    #     while idx < len(query) and query[idx] != "}":
    #         # advance to the start of a triple
    #         idx = advance_idx_thru_space(query, idx)
    #         # advance thru the head
    #         idx = advance_idx_to_space(query, idx)
    #         # advance to the relation
    #         idx = advance_idx_thru_space(query, idx)

    def __call__(self, question: str, postprocess: bool = True):
        question = preprocess_qn(question)

        input_ids = self.tokenizer(question, return_tensors="pt").input_ids.to(
            self.device
        )
        output_ids = self.model.generate(input_ids, max_new_tokens=self.max_new_tokens)
        query = self.tokenizer.decode(output_ids[0], skip_special_tokens=True)

        if postprocess:
            return postprocess_query(query)
        return query
