import os

from keras import Input
from tensorflow import keras
from transformers import TFBertModel
import tensorflow as tf

from Marie.CandidateSelection.location import SCORE_MODEL_DIR
from Marie.Util.Embedding.Embedding import Embedding


class CandidateSelector:

    def __init__(self):
        self.max_len = 20
        self.kg_embedding_size = 113

        self.score_model_dir = SCORE_MODEL_DIR
        self.model = self.build_model()
        self.load_score_model()

        self.embedding_util = Embedding()

    def entity2embedding(self, entity_name):
        return self.embedding_util.name2embedding(entity_name)


    def load_score_model(self):
        self.model.load_weights(os.path.join(self.score_model_dir, "score_model.h5"))

    def build_model(self):
        bert = TFBertModel.from_pretrained('bert-base-cased')
        bert.trainable = True
        # Input 1, the attention mask
        # Input 2, the tokenized question

        # input_ids = Input(shape=(self.max_len,), dtype=tf.int32, name="input_ids")
        input_ids = Input(shape=(20,), dtype=tf.int32, name="input_ids")
        # input_mask = Input(shape=(self.max_len,), dtype=tf.int32, name="attention_mask")
        input_mask = Input(shape=(20,), dtype=tf.int32, name="attention_mask")
        kg_embedding = Input(shape=(self.kg_embedding_size,), dtype=tf.float32, name="kg_embedding")
        # What is the shape of KG embedding? # 113, currently, reduce it to 64

        kg_reduced_layer = keras.layers.Dense(113, activation='relu', name='kg_reduced_layer')(kg_embedding)

        embeddings = bert(input_ids, attention_mask=input_mask)[0]
        embeddings = keras.layers.GlobalMaxPool1D()(embeddings)

        # What is the shape of the embedding? # 768, fixed size
        embedding_reduced_layer_0 = keras.layers.Dense(512, activation='relu')(embeddings)
        embedding_reduced_layer_1 = keras.layers.Dense(256, activation='relu')(embedding_reduced_layer_0)
        embedding_reduced_layer_2 = keras.layers.Dense(128, activation='relu')(embedding_reduced_layer_1)
        embedding_reduced_layer_3 = keras.layers.Dense(113, activation='relu')(embedding_reduced_layer_2)

        joined_vector = keras.layers.add([embedding_reduced_layer_3, kg_reduced_layer])  # both vectors to be 34
        # the output is a score between 0 - 1, telling you the likeness of the question - head entity - tail entity

        additional_layer_1 = keras.layers.Dense(113, activation='relu')(joined_vector)
        additional_layer_1 = tf.keras.layers.Dropout(0.2)(additional_layer_1)
        additional_layer_2 = keras.layers.Dense(113, activation='relu')(additional_layer_1)
        additional_layer_2 = tf.keras.layers.Dropout(0.2)(additional_layer_2)
        additional_layer_3 = keras.layers.Dense(113, activation='relu')(additional_layer_2)
        additional_layer_3 = tf.keras.layers.Dropout(0.2)(additional_layer_3)
        output = keras.layers.Dense(2, activation='softmax')(additional_layer_3)

        # kg_embedding is the embedding of the head entity
        model = tf.keras.Model(inputs=[input_ids, input_mask, kg_embedding], outputs=output)
        # model.summary()
        # keras.utils.plot_model(model, "Full_Model.png", show_shapes=True)
        return model


if __name__ == '__main__':
    my_candidate_selector = CandidateSelector()
