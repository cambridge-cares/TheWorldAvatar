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

from Marie.Util.Embedding.Embedding import Embedding
from Marie.Util.Models.ModelBuilder import ModelBuilder


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

        self.model_builder = ModelBuilder()
        self.model = self.build_model()

    def create_dataset(self):
        # for each e_h, find the embedding the training set contains 1. the question (q), 2. the head entity (e_h),
        # 3. the answer entity (e_t), the embedding of e_h
        #

        dataset = pd.read_csv(self.dataset_path, sep='\t')
        embedding_list = []
        for index, row in dataset.iterrows():
            # get embedding from the mapping
            embedding = self.ent_mapping[row['head']]
            embedding_list.append(embedding)

        embedding_list = pd.DataFrame(embedding_list)
        dataset = pd.concat([dataset, embedding_list], axis=1, ignore_index=True)
        dataset.to_csv('test')

        # anything but the score

        # dataset = dataset.sample(frac=0.2, ignore_index=True)
        data_train = dataset.sample(frac=0.8).reset_index(drop=True)
        data_test = dataset.drop(data_train.index).reset_index(drop=True)
        # question: 2 , e_h : 3, e_t, score : 4
        y_train = data_train.iloc[:, 4]
        y_train = to_categorical(y_train, num_classes=2, dtype='int32')

        y_test = data_test.iloc[:, 4]
        y_test = to_categorical(y_test, num_classes=2, dtype='int32')

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

        tail_entity_train_list = data_train.iloc[:, 3]
        tail_entity_test_list = data_test.iloc[:, 3]

        tail_embedding_train = self.embedding_util.name2embedding_batch(tail_entity_train_list.to_list())
        tail_embedding_test = self.embedding_util.name2embedding_batch(tail_entity_test_list.to_list())

        print(tail_embedding_test)

        cols_to_drop = [0, 1, 2, 3, 4]  # 3 is the tail entity

        kg_embedding_train = data_train.drop(data_train.columns[cols_to_drop], axis=1)
        kg_embedding_test = data_test.drop(data_test.columns[cols_to_drop], axis=1)

        x_train = {'head_embedding': kg_embedding_train, 'tail_embedding': tail_embedding_train,
                   'input_ids': x_question_train['input_ids'],
                   'attention_mask': x_question_train['attention_mask']}

        x_test = {'head_embedding': kg_embedding_test, 'tail_embedding': tail_embedding_test,
                  'input_ids': x_question_test['input_ids'],
                  'attention_mask': x_question_test['attention_mask']}

        return x_train, y_train, x_test, y_test

    def build_model(self):
        return self.model_builder.get_scoring_model()

    def train(self):
        optimizer = Adam(
            learning_rate=0.1)
        # Set loss and metrics
        loss = BinaryCrossentropy()
        metric = BinaryAccuracy('accuracy'),
        self.model.compile(optimizer=optimizer, loss=loss, metrics=metric)
        train_history = self.model.fit(
            x=self.x_train,
            y=self.y_train,
            validation_data=(
                self.x_test, self.y_test
            ),
            epochs=500,
            batch_size=64
        )
        self.model.save_weights('model_weight.h5')


if __name__ == '__main__':
    TSF = CandidateScore()
    TSF.train()
