import json
from fuzzywuzzy import fuzz


class SearchEngine:
    def __init__(self):
        self.file_path = 'wiki_dictionary_new'
        with open(self.file_path) as f:
            self.wiki_dictionary = json.loads(f.read())
        self.top_k = 5

    def find_matches_from_wiki(self, term, mode='instance'):
        high_score_terms = self.find_high_scores(term, mode)
        # get the uri for the terms
        for term in high_score_terms:
            score = term[1]
            term = term[0]
            uri = self.wiki_dictionary[mode]['dict'][term]
            print(uri)

    # three op
    def find_high_scores(self, term, mode='instance'):
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
