from urllib.error import URLError

import nltk
from nltk import word_tokenize
from nltk.corpus import stopwords
import urllib.request
from urllib.parse import quote
import json
import multiprocessing
from nltk.stem.snowball import SnowballStemmer
import JPS_NLQ_SPARQL

import NLTK_formatter
import NLTK_log
import NLTK_CompareString


class NLQ:

    def __init__(self, hmq_object, triples, target_class, condition_obj, sma_object):

        _sparql_engine = JPS_NLQ_SPARQL.SPARQLEngine()
        self.sparql_engine = _sparql_engine
        self.stemmer = SnowballStemmer("english")

        self.jjr_more_map = ['higher', 'taller', 'bigger', 'more', 'larger']
        self.jjr_less_map = ['shorter', 'lower', 'smaller', 'less']

        self.hmq_object = hmq_object
        self.sma_object = sma_object
        self.triples = triples
        self.target_class = target_class
        self.condition_obj = condition_obj

        self.template_for_how_many = '''SELECT distinct (COUNT(?s) AS ?Counter) WHERE {%s ?s ?p <%s> %s}'''
        self.template_for_s_p_o_search = '''SELECT distinct * WHERE {%s ?s ?p <%s>}'''

        self.template_for_condition_component = '''. \n{?s <%s> ?value}
                                                    UNION
                                                    {?s <%s> ?value} 
                                                    FILTER(?value %s %s)'''  # 1: predicate1 2: predicate2 3: sign 4:  value

        self.template_for_hypernym = '''?s <http://purl.org/linguistics/gold/hypernym> <%s> . ?s'''

        self.template_for_all_entities = ''' { ?s <http://purl.org/linguistics/gold/hypernym> <%s> }
                                                UNION  
                                             {{?s <http://purl.org/dc/terms/subject> <%s> } 
                                                UNION
                                            { ?s rdf:type <http://dbpedia.org/ontology/%s>} }'''  # arg1 obj, arg2 Category, arg3 Ontology

        self.template_for_all_entities_predicates = ''' SELECT distinct ?p WHERE {
                                                            { ?s <http://purl.org/linguistics/gold/hypernym> <%s> }
                                                            UNION  
                                                            {
                                                            {?s <http://purl.org/dc/terms/subject> <%s> } 
                                                            UNION
                                                            { ?s rdf:type <%s>} 
                                                            }
                                                            ?s ?p ?o } LIMIT 50 '''  # arg1 obj, arg2 Category, arg3 Ontology

    # Show me all basketball players that are higher than 2 meters
    # Does Neymar play for Real Madrid
    # What is the longest river in Africa and Asia and Europe
    # What is the longest river in China?
    # How many mountains in Asia that are higher than 4000 meters
    # How many rivers are there in Europe?
    def how_many_question(self):
        # construct COUNT component
        # (COUNT(?s) AS ?Counter)
        _has_condition = False
        if self.condition_obj['Types']:
            _has_condition = True
            # Construct condition component
            _sign = '>'
            print('condition_obj', self.condition_obj)
            _type = self.condition_obj['Types'][0]
            if _type == 'value':
                _value = self.condition_obj['value']
                _JJR = self.condition_obj['JJR'][0][0]
                _NP = self.condition_obj['NP'][0][0]
                if _JJR in self.jjr_more_map:
                    _sign = ' > '
                elif _JJR in self.jjr_less_map:
                    _sign = ' < '
            # TODO: _condition_component = self.template_for_condition_component %() # 1: predicate 2: sign 3:  value

        # if there is a triple:
        if self.triples:
            for _triple in self.triples:
                _triple = _triple['triple']
                _subject = _triple[0]
                _object = _triple[1]

                # check types of sub and obj
                # subject is a string
                # ================== Sub ========================
                _subs_result = self.look_up_in_dbpedia(_subject)
                _subs = _subs_result['sorted_uris']
                _highest_sub = _subs[0]
                _subs_category_map = _subs_result['category_map']
                _subs_categories = _subs_category_map[_highest_sub]
                _subject_term_string = self.get_term_string(_subject)
                _highest_sub_category = self.rank_category(_subs_categories, _subject_term_string)
                _highest_sub_ontology = _highest_sub.split('/')[-1]

                print('_highest_sub_category', _highest_sub_category)
                # ===============================================

                # ================== Obj ========================
                _objs_result = self.look_up_in_dbpedia(_object)
                _objs = _objs_result['sorted_uris']
                _highest_obj = _objs[0]
                _objs_category_map = _objs_result['category_map']
                _objs_categories = _objs_category_map[_highest_obj]
                _object_term_string = self.get_term_string(_object)
                _highest_obj_category = self.rank_category(_objs_categories, _object_term_string)
                _highest_obj_ontology = _highest_obj.split('/')[-1]
                print('_highest_obj_category', _highest_obj_category)
                if not _highest_obj_category:
                    _highest_obj_category = 'http://dbpedia.org/resource/Category:' + _object_term_string.replace(' ','_')
                # ===============================================

                _highest_obj = _objs[0]
                print('_highest_obj', _highest_obj)
                print('_highest_sub', _highest_sub)
                # template_for_all_entities
                s_p_o_1 = (self.template_for_s_p_o_search % (_highest_sub, _highest_obj))
                s_p_o_2 = (self.template_for_s_p_o_search % (_highest_obj, _highest_sub))
                # for obj:
                all_entities_for_obj = self.template_for_all_entities % (
                    _highest_obj, _highest_obj_category,
                    _highest_obj_ontology)  # arg1 obj, arg2 Category, arg3 Ontology

                print('all_entities_for_sub', all_entities_for_obj)
                s_p_o_3 = (self.template_for_s_p_o_search % (all_entities_for_obj, _highest_sub))
                how_many_count_query_for_obj = (self.template_for_how_many % (all_entities_for_obj, _highest_sub, ''))

                _predicates = self.search_for_entities_predicates(_highest_obj, _highest_obj_category,
                                                                  _highest_obj_ontology, _NP)

                _conditon_component = self.template_for_condition_component % (
                    _predicates[0], _predicates[1], _sign, _value)  # 1: predicate1 2: predicate2 3: sign 4:  value

                print('_conditon_component', _conditon_component)
                how_many_count_query_for_obj_with_condition = (
                        self.template_for_how_many % (all_entities_for_obj, _highest_sub, _conditon_component))
                print('how_many_count_query_for_obj_with_condition', how_many_count_query_for_obj_with_condition)
                # arg1 obj, arg2 Category, arg3 Ontology

                # TODO: how_many_count_query_for_obj_with_condition = (self.template_for_how_many % (all_entities_for_obj, _highest_sub, ''))

                # print('S-P-O Query 1\n', s_p_o_1.replace('        ', ''))
                # print('==============================================')
                # print()
                # print('S-P-O Query 2\n', s_p_o_2.replace('        ', ''))
                # print('==============================================')
                # print()
                # print('S-P-O Query 3\n', s_p_o_3.replace('        ', ''))
                # print('==============================================')
                # print()
                print('How many for object Query 3\n', how_many_count_query_for_obj.replace('        ', ''))





    def search_for_entities_predicates(self, _obj, _category, _ontology, label_to_compare):
        query_for_all_entities_predicates = self.template_for_all_entities_predicates % (_obj, _category, _ontology)
        print('query_for_all_entities_predicates', query_for_all_entities_predicates)
        results_list = self.sparql_engine.fire_query(query_for_all_entities_predicates)['results']['bindings']
        # print('results_list', results_list)
        return self.rank_predicate(results_list, label_to_compare)

    def rank_predicate(self, predicate_uris, term_to_compare):

        _manager = multiprocessing.Manager()
        _return_dict = _manager.dict()
        _return_dict['scores'] = []
        _return_dict['uris'] = []
        jobs = []
        for _result in predicate_uris:
            _result = _result['p']['value']
            _label = _result.split('/')[-1]
            p = multiprocessing.Process(target=self.nlq_compare_similarity,
                                        args=(_label, term_to_compare, _result, _return_dict))
            jobs.append(p)
            p.start()
        for j in jobs:
            j.join()

        sorted_predicates = [x for _, x in sorted(zip(_return_dict['scores'], _return_dict['uris']), reverse=True)]
        return sorted_predicates[:2]

    def compare_similarity(self):
        print()

    def how_jj_question(self):
        print()

    def construct_sparql_query(self):
        print()

    def get_term_string(self, _obj_to_be_looked):
        if type(_obj_to_be_looked) == type(''):  # it is a string
            string_to_look_up = _obj_to_be_looked
            return string_to_look_up

    def look_up_in_dbpedia(self, obj_to_be_looked):
        string_to_look_up = ''
        if type(obj_to_be_looked) == type(''):  # it is a string
            string_to_look_up = obj_to_be_looked

        url = 'http://lookup.dbpedia.org/api/search/KeywordSearch?QueryClass=&MaxHits=5&QueryString=' + quote(
            string_to_look_up)
        headers = {}
        headers['Accept'] = 'application/json'
        request = urllib.request.Request(url, headers=headers)
        response = urllib.request.urlopen(request)
        json_response = json.loads(response.read().decode('utf-8', 'replace'))
        return self.rank_uri(json_response, string_to_look_up)

    def rank_uri(self, json_response, term):

        _result_list = json_response['results']
        _manager = multiprocessing.Manager()
        return_dict = _manager.dict()

        return_dict['scores'] = []
        return_dict['uris'] = []
        jobs = []

        _categories_map = {}

        for _result in _result_list:
            _label = _result['label']
            _uri = _result['uri']
            _categories = _result['categories']
            _categories_map[_uri] = _categories
            p = multiprocessing.Process(target=self.nlq_compare_similarity, args=(_label, term, _uri, return_dict))
            jobs.append(p)
            p.start()
        for j in jobs:
            j.join()

        sorted_uris = [x for _, x in sorted(zip(return_dict['scores'], return_dict['uris']), reverse=True)]
        return {'sorted_uris': sorted_uris, 'category_map': _categories_map}

    def rank_category(self, _categories, _term):
        _scores = []
        _uris = []
        for _category in _categories:
            _score = self.nlq_compare_similarity(_category['label'], _term, '', '')
            _scores.append(_score)
            _uris.append(_category['uri'])

        sorted_uris = [x for _, x in sorted(zip(_scores, _uris), reverse=True)]
        if sorted_uris:
            return sorted_uris[0]

    def nlq_compare_similarity(self, _label, _term, uri, return_dict):
        if return_dict:
            similarity_score = NLTK_CompareString.phraseSimilarity(_label, _term)
            print(_label, _term, similarity_score)
            return_dict['scores'] += [similarity_score]
            return_dict['uris'] += [uri]
        else:

            similarity_score = NLTK_CompareString.phraseSimilarity(_label, _term)
        return similarity_score

    def sma_questions(self):
        if self.condition_obj['Types']:
            _has_condition = True
            # Construct condition component
            _sign = '>'
            print('condition_obj', self.condition_obj)
            _type = self.condition_obj['Types'][0]
            if _type == 'value':
                _value = self.condition_obj['value']
                _JJR = self.condition_obj['JJR'][0][0]
                _NP = self.condition_obj['NP'][0][0]
                if _JJR in self.jjr_more_map:
                    _sign = ' > '
                elif _JJR in self.jjr_less_map:
                    _sign = ' < '
        if self.triples:
            for _triple in self.triples:
                _triple = _triple['triple']
                _subject = _triple[0]
                _object = _triple[1]

                # check types of sub and obj
                # subject is a string
                # ================== Sub ========================
                _subs_result = self.look_up_in_dbpedia(_subject)
                _subs = _subs_result['sorted_uris']
                _highest_sub = _subs[0]
                _subs_category_map = _subs_result['category_map']
                _subs_categories = _subs_category_map[_highest_sub]
                _subject_term_string = self.get_term_string(_subject)
                _highest_sub_category = self.rank_category(_subs_categories, _subject_term_string)
                _highest_sub_ontology = _highest_sub.split('/')[-1]

                print('_highest_sub_category', _highest_sub_category)
                # ===============================================

                # ================== Obj ========================
                _objs_result = self.look_up_in_dbpedia(_object)
                _objs = _objs_result['sorted_uris']
                _highest_obj = _objs[0]
                _objs_category_map = _objs_result['category_map']
                _objs_categories = _objs_category_map[_highest_obj]
                _object_term_string = self.get_term_string(_object)
                _highest_obj_category = self.rank_category(_objs_categories, _object_term_string)
                _highest_obj_ontology = _highest_obj.split('/')[-1]
                print('_highest_obj_category', _highest_obj_category)
                if not _highest_obj_category:
                    _highest_obj_category = 'http://dbpedia.org/resource/Category:' + _object_term_string.replace(' ','_')
                # ===============================================

                _highest_obj = _objs[0]
                print('_highest_obj', _highest_obj)
                print('_highest_sub', _highest_sub)
                # template_for_all_entities
                s_p_o_1 = (self.template_for_s_p_o_search % (_highest_sub, _highest_obj))
                s_p_o_2 = (self.template_for_s_p_o_search % (_highest_obj, _highest_sub))
                # for obj:
                all_entities_for_obj = self.template_for_all_entities % (
                    _highest_obj, _highest_obj_category,
                    _highest_obj_ontology)  # arg1 obj, arg2 Category, arg3 Ontology

                print('all_entities_for_sub', all_entities_for_obj)
                s_p_o_3 = (self.template_for_s_p_o_search % (all_entities_for_obj, _highest_sub))
                how_many_count_query_for_obj = (self.template_for_how_many % (all_entities_for_obj, _highest_sub, ''))

                _predicates = self.search_for_entities_predicates(_highest_obj, _highest_obj_category,
                                                                  _highest_obj_ontology, _NP)

                _conditon_component = self.template_for_condition_component % (
                    _predicates[0], _predicates[1], _sign, _value)  # 1: predicate1 2: predicate2 3: sign 4:  value

                print('_conditon_component', _conditon_component)
                how_many_count_query_for_obj_with_condition = (
                        self.template_for_how_many % (all_entities_for_obj, _highest_sub, _conditon_component))
                print('how_many_count_query_for_obj_with_condition', how_many_count_query_for_obj_with_condition)
                # arg1 obj, arg2 Category, arg3 Ontology

                # TODO: how_many_count_query_for_obj_with_condition = (self.template_for_how_many % (all_entities_for_obj, _highest_sub, ''))

                # print('S-P-O Query 1\n', s_p_o_1.replace('        ', ''))
                # print('==============================================')
                # print()
                # print('S-P-O Query 2\n', s_p_o_2.replace('        ', ''))
                # print('==============================================')
                # print()
                # print('S-P-O Query 3\n', s_p_o_3.replace('        ', ''))
                # print('==============================================')
                # print()



    def execute(self):
        print('Excuting')
        if self.hmq_object:
            _type = self.hmq_object['type']
            if _type == 'howmany':
                self.how_many_question()
            elif _type == 'howjj':
                print('how jj problem')

        if self.sma_object:
            if self.sma_object['SMA']:
                print('SMA !', self.sma_object)
                self.sma_questions()


# go to the triples
