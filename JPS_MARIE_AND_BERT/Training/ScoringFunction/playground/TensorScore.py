import os

import numpy as np
import pandas as pd
from keras.layers import Dense, Dropout
from keras import Sequential
from keras.utils import to_categorical

abs_path = r'E:\JPS_2022_05\JPS\JPS_MARIE_AND_BERT\Dataset\Supermini\embeddings\transr'


def init_model():
    _model = Sequential()
    # model takes input with dimension of 10, output is 0,1. Need a sigmod

    _model.add(Dense(10, activation='relu', input_shape=(10,)))
    _model.add(Dropout(0.2))
    # _model.add(Dense(20, activation='relu'))
    _model.add(Dense(20, activation=None))
    _model.add(Dropout(0.2))
    _model.add(Dense(1, activation='sigmoid'))
    print(_model.summary())
    _model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['acc'])
    return _model

    # To process the question, use pretrained BERT to do an embedding of the question.
    # The goal is to take the question, the embedding of eh and et as inputs, produce a float from 0 - 1 as the output


def BERT():
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

def load_ent_embedding():
    # 220 entities with 10 dimensions
    # meanwhile, the question is a vector of 24 ... somehow they need to be ...

    ent_embedding_path = os.path.join(abs_path, 'ent_embedding.tsv')
    ent_embedding = pd.read_csv(ent_embedding_path, sep='\t', header=None)
    return ent_embedding


def load_ent_mapping():
    ent_mapping_path = os.path.join(abs_path, 'ent_labels.tsv')
    ent_mapping = pd.read_csv(ent_mapping_path, sep='\t', header=None)
    return ent_mapping


def create_data():
    split_index = 150
    ent_embedding = load_ent_embedding()
    ent_mapping = load_ent_mapping()
    labels = pd.DataFrame([int('_' in ent_label[0]) for ent_label in ent_mapping.values.tolist()])
    _y_train = labels[0:split_index]
    _y_test = labels[split_index:]

    print('====== y ======')
    print(len(_y_train), len(_y_test), len(labels))
    _x_train = ent_embedding[:split_index]
    _x_test = ent_embedding[split_index:]
    print('====== x ======')
    print(_x_train.shape, _x_test.shape, ent_embedding.shape)
    return _x_train, _x_test, _y_train, _y_test


def get_embedding_by_name(_name):
    ent_embedding = load_ent_embedding()
    ent_mapping = load_ent_mapping()
    index = ent_mapping[ent_mapping[0] == _name].index.values[0]
    labels = pd.DataFrame([int('molecular_formula' in ent_label[0]) for ent_label in ent_mapping.values.tolist()])
    print('The supposed label is', labels.iloc[[index]])

    return ent_embedding.iloc[[index]]


# load_ent_embedding(0)
# load_ent_mapping()
x_train, x_test, y_train, y_test = create_data()



model = init_model()
history = model.fit(x=x_train, y=y_train, epochs=500, validation_data=(x_test, y_test))
print(history.history.keys())
print(y_test)
# model.predict()

ent_mapping = load_ent_mapping()

embeddings = []
for name in ent_mapping.values.tolist():
    embedding = get_embedding_by_name(name[0])
    embeddings.append(embedding)

    rst = model.predict(embedding)
    print(rst)

print(y_test.value_counts())

# TODO: Make the prototype question-answer dataset
# TODO:
