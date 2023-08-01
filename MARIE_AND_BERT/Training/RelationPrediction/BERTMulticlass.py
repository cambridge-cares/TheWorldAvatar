# importing the dataset
import numpy as np
import pandas as pd

df_train = pd.read_csv('./input/pubchem/train.txt', header=None, sep=';', names=['Input', 'Sentiment'],
                       encoding='utf-8')
df_test = pd.read_csv('./input/pubchem/test.txt', header=None, sep=';', names=['Input', 'Sentiment'],
                      encoding='utf-8')

# ========================================================
encoded_dict = {'count_hydrogen_bond_acceptor': 0, 'count_hydrogen_bond_donor': 1, 'count_rotatable_bond': 2, 'count_heavy_atoms': 3, 'count_isotope_atom': 4, 'count_covalent_unit': 5, 'compound_id': 6, 'charge': 7, 'compound_complexity': 8, 'fingerprint': 9, 'iupac_name': 10, 'inchi_standard': 11, 'inchi_key': 12, 'log_p': 13, 'exact_mass': 14, 'molecular_formula': 15, 'molecular_weight': 16, 'tpsa': 17, 'canonical_smiles': 18}
# encoded_dict = {'anger': 0, 'fear': 1, 'joy': 2, 'love': 3, 'sadness': 4, 'surprise': 5}
#encoded_dict = {'log_p': 1, 'molar_mass': 0}

max_len = 24

df_train['Sentiment'] = df_train.Sentiment.map(encoded_dict)
df_test['Sentiment'] = df_test.Sentiment.map(encoded_dict)


from keras.utils import to_categorical

y_train = to_categorical(df_train['Sentiment'])
y_test = to_categorical(df_test['Sentiment'])


from transformers import AutoTokenizer, TFBertModel

tokenizer = AutoTokenizer.from_pretrained('bert-base-cased')
bert = TFBertModel.from_pretrained('bert-base-cased')

# Tokenize the input (takes some time)
# here tokenizer using from bert-base-cased
x_train = tokenizer(
    text=df_train.Input.tolist(),
    add_special_tokens=True,
    max_length=max_len,
    truncation=True,
    padding=True,
    return_tensors='tf',
    return_token_type_ids = False,
    return_attention_mask = True,
    verbose = True)
x_test = tokenizer(
    text=df_test.Input.tolist(),
    add_special_tokens=True,
    max_length=max_len,
    truncation=True,
    padding=True,
    return_tensors='tf',
    return_token_type_ids = False,
    return_attention_mask = True,
    verbose = True)

print(x_test)


input_ids = x_train['input_ids']
attention_mask = x_train['attention_mask']

from keras.optimizers import Adam
from keras.losses import CategoricalCrossentropy
from keras.metrics import CategoricalAccuracy
from keras.layers import Input, Dense, concatenate

import tensorflow as tf

number_of_labels = 19

input_ids = Input(shape=(max_len,), dtype=tf.int32, name="input_ids")
input_mask = Input(shape=(max_len,), dtype=tf.int32, name="attention_mask")
xxx = Input(shape=(max_len,), dtype=tf.int32, name='xxx')


embeddings = bert(input_ids,attention_mask = input_mask)[0]
out = tf.keras.layers.GlobalMaxPool1D()(embeddings)
out = Dense(128, activation='relu')(out)
out = tf.keras.layers.Dropout(0.1)(out)
out = Dense(32,activation = 'relu')(out)

xxx_layer = Dense(16, activation='relu')(xxx)


out = concatenate([out, xxx_layer])

y = Dense(number_of_labels,activation = 'sigmoid')(out)
# model = tf.keras.Model(inputs=[input_ids, input_mask], outputs=y)
model = tf.keras.Model(inputs=[input_ids, input_mask, xxx], outputs=y)
model.layers[2].trainable = True
print(model.layers[2])

model.summary()

optimizer = Adam(
    learning_rate=5e-05, # this learning rate is for bert model , taken from huggingface website
    epsilon=1e-08,
    decay=0.01,
    clipnorm=1.0)
# Set loss and metrics
loss =CategoricalCrossentropy(from_logits = True)
metric = CategoricalAccuracy('balanced_accuracy'),
# Compile the model
model.compile(
    optimizer = optimizer,
    loss = loss,
    metrics = metric)

train_history = model.fit(
    x ={'input_ids':x_train['input_ids'],'attention_mask':x_train['attention_mask'], 'xxx':x_train['input_ids']} ,
    y = y_train,
    validation_data = (
    {'input_ids':x_test['input_ids'],'attention_mask':x_test['attention_mask'], 'xxx':x_test['input_ids']}, y_test
    ),
  epochs=10,
    batch_size=32
)

model.save('./model')
model.save_weights('model_weight.h5')


predicted_raw = model.predict({'input_ids':x_test['input_ids'],'attention_mask':x_test['attention_mask']})

y_predicted = np.argmax(predicted_raw, axis = 1)
y_true = df_test.Sentiment

from sklearn.metrics import classification_report
print(classification_report(y_true, y_predicted))



texts = input(str('input the text'))
x_val = tokenizer(
    text=texts,
    add_special_tokens=True,
    max_length=max_len,
    truncation=True,
    padding='max_length',
    return_tensors='tf',
    return_token_type_ids = False,
    return_attention_mask = True,
    verbose = True)
validation = model.predict({'input_ids':x_val['input_ids'],'attention_mask':x_val['attention_mask']})*100
for key , value in zip(encoded_dict.keys(),validation[0]):
    print(key,value)

