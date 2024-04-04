import os
import time
from pprint import pprint

import pandas as pd
from numpy import float32, int32
import tensorflow as tf

from Marie.CandidateSelection.location import SCORE_MODEL_DIR
from Marie.Util.Embedding.Embedding import Embedding
from Marie.Util.Models.archived_models.ModelBuilder import ModelBuilder
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
        return self.embedding_util.ent2embedding(entity_name)

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
        # TODO: 1. find out the length of the candidate_embedding_list

        # num_candidate, fill the head_entity_embedding and tokenized_sentence to its length

        num_candidate = len(candidate_embedding_list)

        input_ids = tf.repeat(input=[tokenized_sentence['input_ids'][0]], repeats=[num_candidate], axis=0)
        attention_mask = tf.repeat(input=[tokenized_sentence['attention_mask'][0]], repeats=[num_candidate], axis=0)
        head_embedding = tf.repeat(input=[head_entity_embedding], repeats=[num_candidate], axis=0)

        print(attention_mask)

        head_embedding = pd.DataFrame(head_embedding, dtype=float32)
        attention_mask = pd.DataFrame(attention_mask, dtype=int32)
        input_ids = pd.DataFrame(input_ids, dtype=int32)
        candidate_embedding_list = pd.DataFrame(candidate_embedding_list, dtype=float32)

        print('type of head embedding')
        print(type(head_embedding))
        print('type of candidate embedding list')
        print(type(candidate_embedding_list))

        START_TIME = time.time()
        predicted_raw = self.model.predict({'input_ids': input_ids,
                                            'attention_mask': attention_mask,
                                            'head_embedding': head_embedding,
                                            'tail_embedding': candidate_embedding_list})
        END_TIME = time.time()
        print('=================== PREDICTION TIME ====================')
        print(END_TIME - START_TIME)

        pprint(predicted_raw)


        # y_predicted = np.argmax(predicted_raw, axis=1)

        # print(y_predicted)
        return predicted_raw


if __name__ == '__main__':
    my_candidate_selector = CandidateSelector()
