# training set: question with e_h and e_t labelled
# other info: embedding


from keras.layers import Dense
from keras import Sequential, Model
from transformers import AutoTokenizer, TFBertModel


class CandidateScore():
    def __init__(self):
        # init the model
        # the model shall take the embedding of q, e_h, e_t and produce a 0 to 1 score for the candidate combination
        # set the correct answer to be 1, incorrect answer to be 0 in the training set.
        self.model = Model()
        self.tokenizer = AutoTokenizer.from_pretrained('bert-base-cased')
        self.bert = TFBertModel.from_pretrained('bert-base-cased')




    def tokenize_question(self, question):
        max_len = 24
        tokenized_question = self.tokenizer(
            text=question,
            add_special_tokens=True,
            max_length=max_len,
            truncation=True,
            padding='max_length',
            return_tensors='tf',
            return_token_type_ids=False,
            return_attention_mask=True,
            verbose=True)
        return tokenized_question

    def get_embedding_by_name(self, name):
        pass
