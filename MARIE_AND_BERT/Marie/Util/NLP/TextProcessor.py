from transformers import AutoTokenizer, TFBertModel


class TextProcessor:

    def __init__(self):
        self.max_len = 20
        self.tokenizer = AutoTokenizer.from_pretrained('bert-base-cased')

    def tokenize_sentence(self, text_list=None):
        max_len = self.max_len
        tokenized_sentence = self.tokenizer(
            text=text_list,
            add_special_tokens=True,
            max_length=max_len,
            truncation=True,
            padding='max_length',
            return_tensors='tf',
            return_token_type_ids=False,
            return_attention_mask=True,
            verbose=True)
        return tokenized_sentence
