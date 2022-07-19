import re
import numpy as np
import pandas as pd
from pprint import pprint

import json
# Gensim
import gensim
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel
from nltk.stem import PorterStemmer
# spacy for lemmatization
import spacy

# Plotting tools
import pyLDAvis
import pyLDAvis.gensim  # don't skip this
import matplotlib.pyplot as plt

# Enable logging for gensim - optional
import logging

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.ERROR)

import warnings

warnings.filterwarnings("ignore", category=DeprecationWarning)

from nltk.corpus import stopwords

stop_words = stopwords.words('english')
stop_words.extend(['from', 'subject', 're', 'edu', 'use'])

with open('../test_corpus/corpus') as f:
    data = json.loads(f.read())
data_words = data
print(data_words)

bigram = gensim.models.Phrases(data_words, min_count=5, threshold=100)  # higher threshold fewer phrases.
trigram = gensim.models.Phrases(bigram[data_words], threshold=100)

# Faster way to get a sentence clubbed as a trigram/bigram
bigram_mod = gensim.models.phrases.Phraser(bigram)
trigram_mod = gensim.models.phrases.Phraser(trigram)

# See trigram example
# print(trigram_mod[bigram_mod[data_words[0]]])

nlp = spacy.load('en_core_web_sm', disable=['parser', 'ner'])

stemmer = PorterStemmer()


def remove_stopwords(texts):
    return [[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]


def make_bigrams(texts):
    return [bigram_mod[doc] for doc in texts]


def make_trigrams(texts):
    return [trigram_mod[bigram_mod[doc]] for doc in texts]


def lemmatization(texts, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV']):
    """https://spacy.io/api/annotation"""
    texts_out = []
    for sent in texts:
        doc = nlp(" ".join(sent))
        texts_out.append([token.lemma_ for token in doc if token.pos_ in allowed_postags])
        # texts_out.append([token.lemma_ for token in doc])
    return texts_out


# Remove Stop Words
data_words_nostops = remove_stopwords(data_words)

# Form Bigrams
data_words_bigrams = make_bigrams(data_words_nostops)

# Initialize spacy 'en' model, keeping only tagger component (for efficiency)
# python3 -m spacy download en

# Do lemmatization keeping only noun, adj, vb, adv
data_lemmatized = lemmatization(data_words_bigrams, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV'])
# Create Dictionary
id2word = corpora.Dictionary(data_lemmatized)

# Create Corpus
texts = data_lemmatized

# Term Document Frequency
corpus = [id2word.doc2bow(text) for text in texts]

# View
print(corpus[:1])

lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,
                                            id2word=id2word,
                                            num_topics=4,
                                            random_state=100,
                                            update_every=1,
                                            chunksize=10,
                                            passes=2000,
                                            alpha='auto',
                                            per_word_topics=True)

pprint(lda_model.print_topics(num_words=10))

lda_model.save('LDA_MODEL_TEST')
doc_lda = lda_model[corpus]

# Compute Perplexity
# question = 'what reaction has ch4 as a reactant'
question = 'what is the heat capacity of h2o2'
# question = 'what do you know about china'
topic_dictionary = {0: 'ontocompchem', 1: 'wiki', 2: 'ontospecies', 3: 'ontokin'}
cmd = ''
while cmd != 'stop':
    cmd = input('type in the question\t\t')
    question = [cmd.split(' ')]
    print('========== make bigrams ==========')
    print(make_bigrams(question))
    print('==================================')

    question = lemmatization(make_bigrams(question))
    print(question)
    print('==================================')
    question = question[0]
    bow = lda_model.id2word.doc2bow(question)
    result = lda_model.get_document_topics(bow)
    topics = [topic_dictionary[i[0]] for i in result]
    print(result)
    print(topics)
