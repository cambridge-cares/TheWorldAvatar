import json
import os.path
from pprint import pprint

from fuzzywuzzy import fuzz
from more_itertools import take

from .locations import LOOKUP_TABS_DIR

class SearchInterface:

    def __init__(self):
        self.topics = [ 'ontocompchem_classes', 'ontocompchem_properties', 'OntoKin_classes', 'OntoKin_properties', 'ontospecies_classes',
                       'ontospecies_properties']

        self.dictionary = {}
        self.lookup_table = {}
        self.path = LOOKUP_TABS_DIR
        for topic in self.topics:
            dict_path  = os.path.join(self.path, '%s_dictionary.json' % topic)
            table_path = os.path.join(self.path, '%s.json' % topic)
            with open(dict_path) as f:
                self.dictionary[topic] = json.loads(f.read())
            with open(table_path) as f0:
                self.lookup_table[topic] = json.loads(f0.read())

    def search_matches(self, _word):
        dict_temp = {}

        for topic in self.dictionary:
            # make a dictionary of words and their scores
            topic_dictionary = self.dictionary[topic]
            for key in topic_dictionary:
                score = fuzz.ratio(_word.lower(), key.lower())
                dict_temp[key] = (score, topic)
        sorted_dict = sorted(dict_temp.items(), key=lambda x: x[1][0], reverse=True)
        # sorted_dict = {(k,v): v for k, v in sorted(dict_temp.items(), key=lambda item: item[0], reverse=True)}
        r = take(5, sorted_dict)
        return r

    def get_first_match(self, _word):
        return self.uri_look_up(None, None, _word)

    def uri_look_up(self, question, classification, _word):

        result = self.search_matches(_word)
        pprint(result)

        first_result = result[0]
        first_topic = first_result[1][1]
        first_label = first_result[0]

        return self.lookup_table[first_topic][first_label][0]
       #  return self.lookup_table[first_topic][first_label][0].split('/')[-1]
