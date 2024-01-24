from pprint import pprint
import json, os
import gensim
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel
from nltk.stem import PorterStemmer
import spacy
import logging
from .location import LDA_DIR

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.DEBUG)

import warnings

warnings.filterwarnings("ignore", category=DeprecationWarning)

from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize


def tokenize_word(sentence):
    return [word_tokenize(sentence)]


class LDAClassifier:
    def __init__(self):
        stop_words = stopwords.words('english')
        stop_words.extend(['from', 'subject', 're', 'edu', 'use'])
        self.stop_words = stopwords.words('english')
        self.stop_words.extend(['from', 'subject', 're', 'edu', 'use'])
        self.lda_model = gensim.models.ldamodel.LdaModel.load(os.path.join(LDA_DIR, 'LDA_MODEL'))
        self.nlp = spacy.load('en_core_web_sm', disable=['parser', 'ner'])
        self.stemmer = PorterStemmer()
        self.topic_dictionary = {0: 'ontocompchem', 1: 'wiki', 2: 'ontospecies', 3: 'ontokin'}
        # pprint(self.lda_model.print_topics(num_words=10))

    def classify(self, question):
        original_question = question
        question = self.lemmatization(tokenize_word(question))[0]
        bow = self.lda_model.id2word.doc2bow(question)
        rst = self.lda_model.get_document_topics(bow)
        return self.lookup_topic(original_question, rst)

    def lemmatization(self, texts, allowed_postags=None):
        """https://spacy.io/api/annotation"""
        if allowed_postags is None:
            allowed_postags = ['NOUN', 'ADJ', 'VERB', 'ADV']
        texts_out = []
        for sent in texts:
            doc = self.nlp(" ".join(sent))
            texts_out.append([token.lemma_ for token in doc if token.pos_ in allowed_postags])
            # texts_out.append([token.lemma_ for token in doc])
        return texts_out

    def lookup_topic(self, question, topics):
        # check whether it is a valid result

        if len(topics) == 4 and (round(topics[0][1], 2) == 0.25):
            # Houston, we have a problem
            # logging.warning('No topic is identified in question {}'.format(question))
            return ['wiki', 'ontokin']
            # return 'ERROR002' # Error 002, no topic is identified.

        else:
            sorted_topics = sorted(topics, key=lambda tup: tup[1], reverse=True)
            sorted_topic_names = []
            for topic in sorted_topics:
                sorted_topic_names.append(self.topic_dictionary[topic[0]])

            if 'wiki' not in sorted_topic_names:
                sorted_topic_names.append('wiki')
            return sorted_topic_names


if __name__ == "__main__":
    lda_classifier = LDAClassifier()
    topics = lda_classifier.classify('what is the molecular weight of benzene')
    pprint(topics)
