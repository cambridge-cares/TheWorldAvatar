import torch
from transformers import BertTokenizer


class NLPTools:
    def __init__(self, tokenizer_name="bert-base-cased"):
        self.tokenizer = BertTokenizer.from_pretrained(tokenizer_name)
        self.max_length = 12
        self.device = torch.device("cpu")
        self.nel_stop_words = ["find", "all", "list", "show", "give", "me"]

    def tokenize_question(self, question, repeat_num):
        """
        :param question: question in text
        :param repeat_num:
        :return:
        """
        question = question.replace(" of ", " ")
        tokenized_question = self.tokenizer(question,
                                            padding='max_length', max_length=self.max_length, truncation=True,
                                            return_tensors="pt")
        attention_mask, input_ids = tokenized_question['attention_mask'], tokenized_question['input_ids']
        attention_mask_batch = attention_mask.repeat(repeat_num, 1).to(self.device)
        input_ids_batch = input_ids.repeat(repeat_num, 1).to(self.device)
        return {'attention_mask': attention_mask_batch, 'input_ids': input_ids_batch}, tokenized_question

    def remove_head_entity(self, _question, _head_entity):
        try:
            return _question.upper().replace(_head_entity.upper(), '').strip().lower()
        except AttributeError:
            return _question.strip().lower()

    def filter_stop_words_for_nel(self, question):
        tokens = question.lower().split(" ")
        tokens = [t for t in tokens if t.lower() not in self.nel_stop_words]
        return " ".join(tokens).strip()