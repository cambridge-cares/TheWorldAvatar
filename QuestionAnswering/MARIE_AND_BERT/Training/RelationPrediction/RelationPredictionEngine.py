import os

from tensorflow import keras
from transformers import AutoTokenizer, TFBertModel
import tensorflow as tf
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.callbacks import EarlyStopping
from tensorflow.keras.initializers import TruncatedNormal
from tensorflow.keras.losses import CategoricalCrossentropy
from tensorflow.keras.metrics import CategoricalAccuracy
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.layers import Input, Dense


class RelationPrediction:

    def __init__(self):
        self.MAX_LEN = 50
        self.model_dir = './'
        self.encoded_dict = {'count_hydrogen_bond_acceptor': 0,
                             'count_hydrogen_bond_donor': 1,
                             'count_rotatable_bond': 2,
                             'count_heavy_atoms': 3,
                             'count_isotope_atom': 4,
                             'count_covalent_unit': 5, 'compound_id': 6, 'charge': 7,
                             'compound_complexity': 8, 'fingerprint': 9, 'iupac_name': 10,
                             'inchi_standard': 11, 'inchi_key': 12, 'log_p': 13,
                             'exact_mass': 14, 'molecular_formula': 15,
                             'molecular_weight': 16, 'tpsa':
                                 17, 'canonical_smiles': 18}

        self.NUM_LABELS = len(self.encoded_dict)

        self.model = self.create_model()
        self.model.load_weights(os.path.join(self.model_dir, "model_weight.h5"))

        self.tokenizer = AutoTokenizer.from_pretrained('bert-base-cased')

    def create_model(self):
        max_len = self.MAX_LEN
        number_of_labels = self.NUM_LABELS
        bert = TFBertModel.from_pretrained('bert-base-cased')
        input_ids = Input(shape=(max_len,), dtype=tf.int32, name="input_ids")
        input_mask = Input(shape=(max_len,), dtype=tf.int32, name="attention_mask")
        embeddings = bert(input_ids, attention_mask=input_mask)[0]
        out = tf.keras.layers.GlobalMaxPool1D()(embeddings)
        out = Dense(128, activation='relu')(out)
        out = tf.keras.layers.Dropout(0.1)(out)
        out = Dense(32, activation='relu')(out)
        y = Dense(number_of_labels, activation='sigmoid')(out)
        model = tf.keras.Model(inputs=[input_ids, input_mask], outputs=y)
        model.layers[2].trainable = True
        return model

    def predict_relation(self, texts):
        x_val = self.tokenizer(
            text=texts,
            add_special_tokens=True,
            max_length=self.MAX_LEN,
            truncation=True,
            padding='max_length',
            return_tensors='tf',
            return_token_type_ids=False,
            return_attention_mask=True,
            verbose=True)
        validation = self.model.predict(
            {'input_ids': x_val['input_ids'], 'attention_mask': x_val['attention_mask']}) * 100
        for key, value in zip(self.encoded_dict.keys(), validation[0]):
            print(key, value)


if __name__ == '__main__':
    rpe = RelationPrediction()
    rpe.predict_relation('what is the molecular weight of benzene')
