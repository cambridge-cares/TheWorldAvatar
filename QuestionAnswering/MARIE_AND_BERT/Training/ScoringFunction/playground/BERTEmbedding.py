import itertools
from pprint import pprint

import pandas as pd
import tensorflow as tf
from transformers import TFBertModel, AutoTokenizer
from keras import Model, Input

# lets try out using the BERT model to produce an embedding


# INIT THE BERT COMPONENT

tokenizer = AutoTokenizer.from_pretrained('bert-base-cased')
bert = TFBertModel.from_pretrained('bert-base-cased')

max_len = 32

input_ids = Input(shape=(max_len,), dtype=tf.int32, name="input_ids")
input_mask = Input(shape=(max_len,), dtype=tf.int32, name="attention_mask")

embeddings = bert(input_ids, attention_mask=input_mask)[0]
embeddings = tf.keras.layers.GlobalMaxPool1D()(embeddings)
model = tf.keras.Model(inputs=[input_ids, input_mask], outputs=embeddings)


def embed_sentence(sentence):
    # tokenize the sentence
    x_val = tokenizer(
        text=sentence,
        add_special_tokens=True,
        max_length=max_len,
        truncation=True,
        padding='max_length',
        return_tensors='tf',
        return_token_type_ids=False,
        return_attention_mask=True,
        verbose=True)

    embeddings = model.predict({'input_ids': x_val['input_ids'], 'attention_mask': x_val['attention_mask']})[0]
    df = pd.DataFrame(embeddings) # 768 = 24(max_len) * 32
    print('##################### The shape of the embedding #########################')
    print(df.shape)
    return tf.convert_to_tensor(embeddings)


sentences = ['a whale lives in the sea', 'a fish also live in the ocean', 'a bird flies in the sky',
             'holly shit this is irrelevant', 'a fish swims in the water']

all_embeddings = {}
for s in sentences:
    embedded_sentence = embed_sentence(s)
    all_embeddings[s] = embedded_sentence

all_pairs = [sorted(l) for l in itertools.permutations(sentences, 2)]
used_pair = []
score_board = {}

for pair in all_pairs:
    s_1 = pair[0]
    s_2 = pair[1]

    e_1 = all_embeddings[s_1]
    e_2 = all_embeddings[s_2]
    if pair not in used_pair:
        similarity = 1 / tf.math.reduce_sum(tf.norm(e_1 - e_2, axis=-1))
        used_pair.append(pair)
        score_board[' | '.join(pair)] = float(similarity)

        # print(f'{s_1} | {s_2} | {similarity}')

score_board = dict(sorted(score_board.items(), key=lambda item: item[1], reverse=True))
print(score_board)

