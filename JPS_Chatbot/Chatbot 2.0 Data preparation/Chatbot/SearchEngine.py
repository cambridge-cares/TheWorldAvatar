import json
from fuzzywuzzy import fuzz


def remove_duplicated(uris):
    temp = []
    result = []
    for uri in uris:
        if uri not in temp:
            result.append(uri)
        temp.append(uri)
    return result


class SearchEngine:
    def __init__(self):
        self.file_path = '../search_engine/wiki_dictionary'
        with open(self.file_path) as f:
            self.wiki_dictionary = json.loads(f.read())
        self.top_k = 3

    def find_matches_from_wiki(self, term, mode='entity'):
        high_score_terms = self.find_high_scores(term, mode)
        # get the uri for the terms
        uris = []
        for term in high_score_terms:
            score = term[1]
            term = term[0]
            uri = self.wiki_dictionary[mode]['dict'][term]
            print(uri)
            for u in uri:
                uris.append(u)
        return uris

    # three op
    def find_high_scores(self, term, mode='entity'):
        dictionary = self.wiki_dictionary[mode]['dict']
        table = self.wiki_dictionary[mode]['list']

        # make a list of tuples including the term and the score
        temp = {}
        for word in table:
            temp[word] = fuzz.ratio(term.lower(), word.lower())

        # sort the list by the score, select top five self.top_k
        sort_orders = sorted(temp.items(), key=lambda x: x[1], reverse=True)
        selected_terms = sort_orders[:self.top_k]
        print(selected_terms)
        return selected_terms

        # fuzz.ratio(term.lower(), Str2.lower())

    def parse_entities(self, entities):
        # {'entities': {'attribute': 'molecular weight', 'entity': 'benzene'},
        #  'type': 'item_attribute_query'}
        print('========== entities ============')
        print(entities)
        question_type = entities['type']
        list_of_entities = entities['entities']
        print(list_of_entities)
        results = []
        for key, value in list_of_entities.items():
            if key == 'comparison' or key == 'numerical_value':
                pass
            else:
                if type(value) is list:
                    for v in value:
                        uris = self.find_matches_from_wiki(term=v, mode=key)
                        obj_temp = {key: remove_duplicated(uris)}
                        results.append(obj_temp)
                else:
                    uris = self.find_matches_from_wiki(term=value, mode=key)
                    obj_temp = {key: remove_duplicated(uris)}
                    results.append(obj_temp)

        return {'intent': question_type, 'entities': results}

