import os

import numpy as np
from keras import Input
from tensorflow import keras
from transformers import TFBertModel
import tensorflow as tf

from Marie.CandidateSelection.location import SCORE_MODEL_DIR
from Marie.Util.Embedding.Embedding import Embedding
from Marie.Util.Models.ModelBuilder import ModelBuilder
from Marie.Util.NLP.TextProcessor import TextProcessor


def build_scoring_model():
    model_builder = ModelBuilder()
    return model_builder.get_scoring_model()


class CandidateSelector:

    def __init__(self):
        self.max_len = 20
        self.kg_embedding_size = 113
        self.model = build_scoring_model()
        self.load_score_model()
        self.embedding_util = Embedding()
        self.text_processor = TextProcessor()

    def entity2embedding(self, entity_name):
        return self.embedding_util.name2embedding(entity_name)

    def entity2embedding_batch(self, entity_name_list):
        return [self.entity2embedding(key) for key in entity_name_list]

    def load_score_model(self):
        self.model.load_weights(os.path.join(SCORE_MODEL_DIR, "score_model.h5"))

    def predict(self, head_entity, sentence, candidate_list):
        _tokenized_sentence = self.text_processor.tokenize_sentence([sentence])
        _head_entity_embedding = self.entity2embedding(head_entity)
        _candidate_embedding_list = self.entity2embedding_batch(candidate_list)
        return self.rank_candidate(head_entity_embedding=_head_entity_embedding,
                                   tokenized_sentence=_tokenized_sentence,
                                   candidate_embedding_list=_candidate_embedding_list
                                   )

    def rank_candidate(self, head_entity_embedding, tokenized_sentence, candidate_embedding_list):
        predicted_raw = self.model.predict({'input_ids': tokenized_sentence['input_ids'],
                                            'attention_mask': tokenized_sentence['attention_mask']}) * 100
        y_predicted = np.argmax(predicted_raw, axis=1)

        print(y_predicted)
        return y_predicted


if __name__ == '__main__':
    my_candidate_selector = CandidateSelector()
