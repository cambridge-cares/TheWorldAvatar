import os

import numpy as np
import pandas as pd
from keras.layers import Dense, Dropout
from keras import Sequential
from keras.utils import to_categorical


class TensorScore():

    def __init__(self):
        self.abs_path = r'E:\JPS_2022_05\JPS\JPS_MARIE_AND_BERT\Dataset\PubchemMiniFull\embeddings\transe'
        self.ent_embedding = self.load_ent_embedding()
        self.ent_mapping = self.load_ent_mapping()
        self.x_train, self.x_test, self.y_train, self.y_test, self.name2id, self.id2name, self.id2label = self.create_data()
        self.model = self.init_model()

    def init_model(self):
        _model = Sequential()
        # model takes input with dimension of 10, output is 0,1. Need a sigmod
        _model.add(Dense(10, activation='relu', input_shape=(113,)))
        _model.add(Dropout(0.6))
        # _model.add(Dense(20, activation='relu'))
        _model.add(Dense(20, activation=None))
        _model.add(Dropout(0.6))
        _model.add(Dense(2, activation='sigmoid'))
        print(_model.summary())
        _model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['acc'])
        return _model

        # To process the question, use pretrained BERT to do an embedding of the question. The goal is to take the
        # question, the embedding of eh and et as inputs, produce a float from 0 - 1 as the output

    def BERT(self):
        from transformers import AutoTokenizer, TFBertModel

        tokenizer = AutoTokenizer.from_pretrained('bert-base-cased')
        bert = TFBertModel.from_pretrained('bert-base-cased')
        texts = '''what's the molecular weight of benzene?'''

        max_len = 24
        x_val = tokenizer(
            text=texts,
            add_special_tokens=True,
            max_length=max_len,
            truncation=True,
            padding='max_length',
            return_tensors='tf',
            return_token_type_ids=False,
            return_attention_mask=True,
            verbose=True)

        print(x_val)

    # def embed_question(question):

    # TODO: somehow load the KG embedding from the training result ...

    def load_ent_embedding(self):
        # 220 entities with 10 dimensions
        # meanwhile, the question is a vector of 24 ... somehow they need to be ...
        ent_embedding_path = os.path.join(self.abs_path, 'ent_embedding.tsv')
        ent_embedding = pd.read_csv(ent_embedding_path, sep='\t', header=None)
        return ent_embedding

    def load_ent_mapping(self):
        ent_mapping_path = os.path.join(self.abs_path, 'ent_labels.tsv')
        ent_mapping = pd.read_csv(ent_mapping_path, sep='\t', header=None)
        return ent_mapping

    def create_data(self):
        labels = pd.DataFrame([int('_' in ent_label[0]) for ent_label in self.ent_mapping.values.tolist()])
        # join the two dataframes.
        ent_embedding = pd.concat([self.ent_embedding, labels], axis=1, ignore_index=True)

        _name2id = {k: v for v, k in enumerate([l[0] for l in self.ent_mapping.values.tolist()])}
        _id2label = labels.values.tolist()
        _id2name = None
        # ok, now follow the standard methods ...
        # 1. split the dataset into two sets randomly ...

        # split the dataset by the label

        positive_set = ent_embedding[ent_embedding.iloc[:, 113] == 1]
        negative_set = ent_embedding.drop(positive_set.index)

        # make sure the positive set and negative set are balanced ...
        # positive set is larger ...

        # sample len(negative) that many from positive ...
        neg_len = negative_set.shape[0]
        positive_set = positive_set.sample(neg_len)

        # both positive set and negative sets are ready
        train_neg_set = negative_set.sample(frac=0.8)
        test_neg_set = negative_set.drop(train_neg_set.index)

        train_pos_set = positive_set.sample(frac=0.8)
        test_pos_set = positive_set.drop(train_pos_set.index)

        train_set = pd.concat([train_pos_set, train_neg_set], axis=0, ignore_index=True)
        test_set = pd.concat([test_neg_set, test_pos_set], axis=0, ignore_index=True)
        # test_set = ent_embedding.drop(train_set.index)

        print(train_set)
        _y_train = to_categorical(train_set.drop(axis=1, columns=range(0, 113)))
        _x_train = train_set.drop(axis=1, columns=113)

        _y_test =  to_categorical(test_set.drop(axis=1, columns=range(0, 113)))
        _x_test = test_set.drop(axis=1, columns=113)

        # lets try to balance the training set ...

        # print('value count of train set: ', _y_train.value_counts())
        # print('value count of test set: ', _y_test.value_counts())
        return _x_train, _x_test, _y_train, _y_test, _name2id, _id2name, _id2label

    def get_embedding_by_name(self, _name):
        _id = self.name2id[_name]
        return self.ent_embedding.iloc[[_id]], _name, self.id2label[_id][0]

    def train(self):
        history = self.model.fit(x=self.x_train, y=self.y_train, epochs=500,
                                 validation_data=(self.x_test, self.y_test))

    def predict(self, _name):
        embedding, idx, label = self.get_embedding_by_name(_name)
        rst = self.model.predict(embedding)
        predicted_rst = np.argmax(rst, axis = 1)
        print('%s has label %s, predicted label %s' % (_name, label, predicted_rst))


if __name__ == '__main__':
    TS = TensorScore()
    TS.train()
    TS.predict('CID9')
    TS.predict('CID79_canonical_smiles')
