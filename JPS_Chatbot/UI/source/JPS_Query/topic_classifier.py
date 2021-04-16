from gensim.models import LdaModel
import os.path

from nltk.stem import PorterStemmer

from .locations import TOPIC_CLASSIFIERS_DIR

class TopicClassifier:
    def __init__(self):
        self.model_path = os.path.join(TOPIC_CLASSIFIERS_DIR,'lda.model')
        self.lda = LdaModel.load(self.model_path)
        self.topic_dic = {1: 'jps', 2: 'jps', 0: 'wiki'}
        self.stemmer = PorterStemmer()

    def get_topics(self, results):
        results = sorted(results, key=lambda x: x[1])
        print(results)
        topics = []
        for topic in results:
            if topic[1] >= 0.8:
                return [self.topic_dic[topic[0]]]
            else:
                topics.append((self.topic_dic[topic[0]], topic[1]))

        temp_dict = {}
        for topic in topics:
            if topic[0] in temp_dict:
                temp_dict[topic[0]] = topic[1] + temp_dict[topic[0]]
            else:
                temp_dict[topic[0]] = topic[1]

        temp_dict = [k for k, v in sorted(temp_dict.items(), key=lambda item: item[1], reverse=True)]
        return temp_dict

    #  DONE: combine the two JPS together ... reverse the score
    def classify_topic(self, question):
        question = [self.stemmer.stem(w) for w in question.split(' ')]
        bow = self.lda.id2word.doc2bow(question)
        result = self.lda.get_document_topics(bow)
        return self.get_topics(result)

# tc = TopicClassifier()
# q = 'reaction contains'
#
# t = tc.classify_topic(stemmer.stem(q))
# print(t)
