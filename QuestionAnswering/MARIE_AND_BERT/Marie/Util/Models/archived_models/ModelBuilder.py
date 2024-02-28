from keras import Input, Model
from keras.layers import concatenate, Flatten, Dense
from keras.losses import CategoricalCrossentropy
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

        input_ids = Input(shape=(20,), dtype=tf.int32, name="input_ids")
        input_mask = Input(shape=(20,), dtype=tf.int32, name="attention_mask")
        head_embedding = Input(shape=(self.kg_embedding_size,), dtype=tf.float32, name="head_embedding")
        tail_embedding = Input(shape=(self.kg_embedding_size,), dtype=tf.float32, name="tail_embedding")

        # create three branched models and concat them, each reduce the output to 24

        # the bert_model: four layers, 768 -> 24

        embeddings = bert(input_ids, attention_mask=input_mask)[0]
        pooled_embedding = keras.layers.GlobalMaxPool1D()(embeddings)
        bert_out = Dense(256, activation='relu')(pooled_embedding)
        bert_out = Dense(128, activation='relu')(bert_out)
        bert_out = Dense(64, activation='relu')(bert_out)
        bert_output = Dense(32, activation='relu')(bert_out)
        BERT_branch_model = Model(inputs=[input_ids, input_mask], outputs=bert_output)

        ################################################################################################

        head_out = Dense(64, activation='relu')(head_embedding)
        head_output = Dense(32, activation='relu')(head_out)
        Head_branch_model = Model(inputs=[head_embedding], outputs=head_output)

        ################################################################################################

        tail_out = Dense(64, activation='relu')(tail_embedding)
        tail_output = Dense(32, activation='relu')(tail_out)
        Tail_branch_model = Model(inputs=[tail_embedding], outputs=tail_output)

        with tf.GradientTape() as tape:
            joined_vector = concatenate([BERT_branch_model.output, Head_branch_model.output, Tail_branch_model.output])

        additional_layer_2 = keras.layers.Dense(64, activation='relu')(joined_vector)
        additional_layer_2 = tf.keras.layers.Dropout(0.2)(additional_layer_2)
        additional_layer_3 = keras.layers.Dense(32, activation='relu')(additional_layer_2)
        additional_layer_3 = tf.keras.layers.Dropout(0.2)(additional_layer_3)
        output = keras.layers.Dense(2, activation='sigmoid')(additional_layer_3)

        # kg_embedding is the embedding of the head entity
        model = tf.keras.Model(inputs=[input_ids, input_mask, head_embedding, tail_embedding], outputs=output)
        model.layers[2].trainable = True
        # model.layers[3].trainable = False
        model.summary()
        # keras.utils.plot_model(model, "Full_Model.png", show_shapes=True)
        return model

    def describe_model(self):
        _model = self.get_scoring_model()
        _model.summary()
        # keras.utils.plot_model(_model, "Full_Model.png", show_shapes=True)


if __name__ == '__main__':
    my_model_builder = ModelBuilder()
    my_model_builder.describe_model()
