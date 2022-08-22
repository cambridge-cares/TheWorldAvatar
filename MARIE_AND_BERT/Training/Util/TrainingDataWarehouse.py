import os

import pandas as pd
from keras.utils import to_categorical

from Marie.Util.Embedding.Embedding import Embedding
from Training.ScoringFunction.playground.location import DATASET_DIR, PLAYGROUND_DIR
from transformers import AutoTokenizer, TFBertModel


class TrainingDataWarehouse:
    def __init__(self):
        self.tokenizer = AutoTokenizer.from_pretrained('bert-base-cased')

    def get_pubchem_score_data(self, frac=1):
        embedding_util = Embedding()
        ent_mapping = embedding_util.load_ent_mapping()
        max_len = 20

        dataset_path = os.path.join(PLAYGROUND_DIR, r'question_set_full')
        dataset = pd.read_csv(dataset_path, sep='\t')
        embedding_list = []
        for index, row in dataset.iterrows():
            # get embedding from the mapping
            embedding = ent_mapping[row['head']]
            embedding_list.append(embedding)

        embedding_list = pd.DataFrame(embedding_list)
        dataset = pd.concat([dataset, embedding_list], axis=1, ignore_index=True)
        dataset.to_csv('test')

        # anything but the score

        # dataset = dataset.sample(frac=0.1).reset_index()
        dataset = dataset.sample(frac=frac, ignore_index=True)
        data_train = dataset.sample(frac=0.8)
        data_test = dataset.drop(data_train.index)

        # question: 2 , e_h : 3, e_t, score : 4

        y_train = data_train.iloc[:, 4].sort_index()
        y_train_categorical = to_categorical(y_train, num_classes=2, dtype='int32')

        y_test = data_test.iloc[:, 4].sort_index()
        y_test_value_count = y_test.value_counts()
        y_test_categorical = to_categorical(y_test, num_classes=2, dtype='int32')

        print('=========== y test value counts ===========')
        print(y_test_value_count)
        x_question_train = self.tokenizer(
            text=data_train.iloc[:, 1].tolist(),
            add_special_tokens=True,
            max_length=max_len,
            truncation=True,
            padding=True,
            return_tensors='tf',
            return_token_type_ids=False,
            return_attention_mask=True,
            verbose=True)

        x_question_test = self.tokenizer(
            text=data_test.iloc[:, 1].tolist(),
            add_special_tokens=True,
            max_length= max_len,
            truncation=True,
            padding=True,
            return_tensors='tf',
            return_token_type_ids=False,
            return_attention_mask=True,
            verbose=True)

        tail_entity_train_list = data_train.iloc[:, 3]
        tail_entity_test_list = data_test.iloc[:, 3]

        tail_embedding_train = embedding_util.name2embedding_batch(tail_entity_train_list.to_list())
        tail_embedding_test = embedding_util.name2embedding_batch(tail_entity_test_list.to_list())

        cols_to_drop = [0, 1, 2, 3, 4]  # 3 is the tail entity

        kg_embedding_train = data_train.drop(data_train.columns[cols_to_drop], axis=1)
        kg_embedding_test = data_test.drop(data_test.columns[cols_to_drop], axis=1)

        x_train = {'head_embedding': kg_embedding_train, 'tail_embedding': tail_embedding_train,
                   'input_ids': x_question_train['input_ids'],
                   'attention_mask': x_question_train['attention_mask']}

        x_test = {'head_embedding': kg_embedding_test, 'tail_embedding': tail_embedding_test,
                  'input_ids': x_question_test['input_ids'],
                  'attention_mask': x_question_test['attention_mask']}

        # return x_train, y_train_categorical, x_test, y_test_categorical
        return x_train, y_train, x_test, y_test


if __name__ == '__main__':
    my_datawarehouse = TrainingDataWarehouse()
    my_datawarehouse.get_pubchem_score_data()