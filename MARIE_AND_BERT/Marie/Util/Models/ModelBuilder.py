from keras import Input
from transformers import TFBertModel
import tensorflow as tf
from tensorflow import keras


class ModelBuilder:
    def __init__(self):
        self.kg_embedding_size = 113

    def get_scoring_model(self):
        bert = TFBertModel.from_pretrained('bert-base-cased')
        # Input 1, the attention mask
        # Input 2, the tokenized question

        # input_ids = Input(shape=(self.max_len,), dtype=tf.int32, name="input_ids")
        input_ids = Input(shape=(20,), dtype=tf.int32, name="input_ids")
        # input_mask = Input(shape=(self.max_len,), dtype=tf.int32, name="attention_mask")
        input_mask = Input(shape=(20,), dtype=tf.int32, name="attention_mask")
        head_embedding = Input(shape=(self.kg_embedding_size,), dtype=tf.float32, name="head_embedding")
        tail_embedding = Input(shape=(self.kg_embedding_size,), dtype=tf.float32, name="tail_embedding")
        # What is the shape of KG embedding? # 113, currently, reduce it to 64

        # another

        # head_reduced_layer = tf.keras.layers.Dense(113, activation='relu', name='head_reduced_layer')(head_embedding)
        # tail_reduced_layer = tf.keras.layers.Dense(113, activation='relu', name='tail_reduced_layer')(tail_embedding)

        embeddings = bert(input_ids, attention_mask=input_mask)[0]
        embeddings = keras.layers.GlobalMaxPool1D()(embeddings)

        # What is the shape of the sentence embedding? # 768, fixed size
        sentence_embedding_reduced_layer_0 = tf.keras.layers.Dense(113, activation='relu')(embeddings)
        # sentence_embedding_reduced_layer_1 = keras.layers.Dense(256, activation='relu')(
        #     sentence_embedding_reduced_layer_0)
        # sentence_embedding_reduced_layer_2 = keras.layers.Dense(128, activation='relu')(
        #     sentence_embedding_reduced_layer_1)
        # sentence_embedding_reduced_layer_3 = keras.layers.Dense(113, activation='relu')(
        #     sentence_embedding_reduced_layer_2)

        joined_vector = keras.layers.add(
            [sentence_embedding_reduced_layer_0, head_embedding, tail_embedding])  # both vectors to be 34
        # the output is a score between 0 - 1, telling you the likeness of the question - head entity - tail entity

        # additional_layer_1 = keras.layers.Dense(113, activation='relu')(joined_vector)
        # additional_layer_1 = tf.keras.layers.Dropout(0.1)(additional_layer_1)
        additional_layer_2 = keras.layers.Dense(64, activation='relu')(joined_vector)
        additional_layer_2 = tf.keras.layers.Dropout(0.1)(additional_layer_2)
        additional_layer_3 = keras.layers.Dense(32, activation='relu')(additional_layer_2)
        additional_layer_3 = tf.keras.layers.Dropout(0.1)(additional_layer_3)
        output = keras.layers.Dense(2, activation='softmax')(additional_layer_3)

        # kg_embedding is the embedding of the head entity
        model = tf.keras.Model(inputs=[input_ids, input_mask, head_embedding, tail_embedding], outputs=output)
        # model.summary()
        # keras.utils.plot_model(model, "Full_Model.png", show_shapes=True)
        return model

    def describe_model(self):
        _model = self.get_scoring_model()
        _model.summary()
        keras.utils.plot_model(_model, "Full_Model.png", show_shapes=True)


if __name__ == '__main__':
    my_model_builder = ModelBuilder()
    my_model_builder.describe_model()
