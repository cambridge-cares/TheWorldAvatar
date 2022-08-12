# training set: question with e_h and e_t labelled
# other info: embedding
import os

import keras.layers
import numpy as np
from keras.layers import Dense
from keras import Sequential, Model, Input, utils
from keras.metrics import CategoricalAccuracy, BinaryAccuracy
from transformers import AutoTokenizer, TFBertModel
import tensorflow as tf
from location import DATASET_DIR
import pandas as pd
from keras.optimizers import Adam
from keras.losses import BinaryCrossentropy, CategoricalCrossentropy
from keras.utils import to_categorical

from Marie.Util.Embedding import Embedding


class CandidateScore():
    def __init__(self):
        # init the model
        # the model shall take the embedding of q, e_h, e_t and produce a 0 to 1 score for the candidate combination
        # set the correct answer to be 1, incorrect answer to be 0 in the training set.
        self.model = Model()
        self.tokenizer = AutoTokenizer.from_pretrained('bert-base-cased')
        self.bert = TFBertModel.from_pretrained('bert-base-cased')
        self.max_len = 20
        self.kg_embedding_size = 113

        self.embedding_util = Embedding()

        self.embedding_path = os.path.join(DATASET_DIR, r'PubchemMiniFull\embeddings\transe')
        self.ent_embedding = self.embedding_util.load_ent_embedding()
        self.ent_mapping = self.embedding_util.load_ent_mapping()

        self.dataset_path = r'question_set_full'

        self.x_train, self.y_train, self.x_test, self.y_test = self.create_dataset()

        self.model = self.build_model()

    def create_dataset(self):
        # for each e_h, find the embedding the training set contains 1. the question (q), 2. the head entity (e_h),
        # 3. the answer entity (e_t), the embedding of e_h
        #

        # TODO: print out the original dataset
        dataset = pd.read_csv(self.dataset_path, sep='\t')

        # dataset = dataset.sample(frac=0.5)
        # print(dataset_part_1)
        # print(dataset_part_1['question'])
        # print(dataset_part_1['head'])
        # print(dataset_part_1['tail'])
        # print(dataset_part_1['score'])
        # 5 - 117
        embedding_list = []
        for index, row in dataset.iterrows():
            # get embedding from the mapping
            embedding = self.ent_mapping[row['head']]
            embedding_list.append(embedding)

        embedding_list = pd.DataFrame(embedding_list)
        dataset = pd.concat([dataset, embedding_list], axis=1, ignore_index=True)
        dataset.to_csv('test')
        # TODO: read the embedding using the name, make sure the embedding is in one column

        # anything but the score

        # dataset = dataset.sample(frac=0.05, ignore_index=True)
        data_train = dataset.sample(frac=0.8).reset_index(drop=True)
        data_test = dataset.drop(data_train.index).reset_index(drop=True)
        # question: 2 , e_h : 3, e_t, score : 4
        y_train = data_train.iloc[:, 4]
        print(y_train[0])
        y_train = to_categorical(y_train, num_classes=2, dtype='int32')
        print(y_train[0])

        y_test = data_test.iloc[:, 4]
        print(y_test[0])
        y_test = to_categorical(y_test, num_classes=2, dtype='int32')
        print(y_test[0])

        x_question_train = self.tokenizer(
            text=data_train.iloc[:, 1].tolist(),
            add_special_tokens=True,
            max_length=self.max_len,
            truncation=True,
            padding=True,
            return_tensors='tf',
            return_token_type_ids=False,
            return_attention_mask=True,
            verbose=True)

        x_question_test = self.tokenizer(
            text=data_test.iloc[:, 1].tolist(),
            add_special_tokens=True,
            max_length=self.max_len,
            truncation=True,
            padding=True,
            return_tensors='tf',
            return_token_type_ids=False,
            return_attention_mask=True,
            verbose=True)

        cols_to_drop = [0, 1, 2, 3, 4]
        kg_embedding_train = data_train.drop(data_train.columns[cols_to_drop], axis=1)
        kg_embedding_test = data_test.drop(data_test.columns[cols_to_drop], axis=1)

        x_train = {'kg_embedding': kg_embedding_train, 'input_ids': x_question_train['input_ids'],
                   'attention_mask': x_question_train['attention_mask']}

        x_test = {'kg_embedding': kg_embedding_test, 'input_ids': x_question_test['input_ids'],
                  'attention_mask': x_question_test['attention_mask']}


        return x_train, y_train, x_test, y_test

    # def load_ent_embedding(self):
    #     # 220 entities with 10 dimensions
    #     # meanwhile, the question is a vector of 24 ... somehow they need to be ...
    #     ent_embedding_path = os.path.join(self.embedding_path, 'ent_embedding.tsv')
    #     ent_embedding = pd.read_csv(ent_embedding_path, sep='\t', header=None)
    #     return ent_embedding
    #
    # def load_ent_mapping(self):
    #     ent_mapping_path = os.path.join(self.embedding_path, 'ent_labels.tsv')
    #     ent_mapping = pd.read_csv(ent_mapping_path, sep='\t', header=None)
    #
    #     name_embedding_mapping = {}
    #     label_list = [ent_label[0] for ent_label in ent_mapping.values.tolist()]
    #     # TODO: make the mapping to be {"CID": 1,1,1,1,1,1,}
    #     # self.ent_embedding
    #     # TODO:
    #     print('================= ent mapping ==================')
    #     print(self.ent_embedding)
    #     for index, embedding in self.ent_embedding.iterrows():
    #         embedding_list = embedding.to_list()
    #         entity_label = label_list[index]
    #         name_embedding_mapping[entity_label] = embedding_list
    #     return name_embedding_mapping

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

    def train(self):
        optimizer = Adam(
            learning_rate=0.00001)
        # Set loss and metrics
        loss = BinaryCrossentropy()
        metric = BinaryAccuracy('balanced_accuracy'),
        self.model.compile(optimizer=optimizer, loss=loss, metrics=metric)
        train_history = self.model.fit(
            x=self.x_train,
            y=self.y_train,
            validation_data=(
                self.x_test, self.y_test
            ),
            epochs=500,
            batch_size=128
        )
        self.model.save_weights('model_weight.h5')

    def tokenize_question(self, question):
        max_len = self.max_len
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


if __name__ == '__main__':
    TSF = CandidateScore()
    TSF.train()
