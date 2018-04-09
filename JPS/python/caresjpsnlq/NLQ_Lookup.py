import urllib.request
from urllib.parse import quote
import json
import pprint
import NLP_CompareString as compareString
from NLQ_SPARQL import SPARQLEngine
import re
import config


class LookUpService:

    def __init__(self):
        self.ols_base_url = 'http://localhost:8080/api/search?q='
        self.SPARQLEngine = SPARQLEngine()


    @staticmethod
    def get_predicate_uri(term, root_sub, root_sub_type):
        # check whether it is in ols
        # ols_result = LookUpService.fake_lookup_in_ols(term)
        ols_result = LookUpService.look_up_in_ols(term)

        if ols_result:
            ols_result['from'] = 'ols'
            return ols_result
        else:
            return LookUpService.get_predicate_in_dbpedia(term, root_sub, root_sub_type)

    @staticmethod
    def get_sub_class_uri(term):
        # 1. search ols, if return 0, go dbpedia and get a class definition
        #        ols_result = LookUpService.fake_lookup_in_ols(term)
        ols_result = LookUpService.look_up_in_ols(term)

        if ols_result:
            return ols_result
        else:
            return LookUpService.get_class_definition_in_dbpedia(term)
        pass

    @staticmethod
    def get_sub_instance_uri(term):
        # ols_result = LookUpService.fake_lookup_in_ols(term)
        ols_result = LookUpService.look_up_in_ols(term)

        if ols_result:
            return ols_result
        else:
            return LookUpService.get_instance_in_dbpedia(term)

    @staticmethod
    def general_url_request(full_url):
        """Simply how to make a url request in python, giving the full quoted requets url -> response in json format"""
        headers = {}
        headers['Accept'] = 'application/json'
        request = urllib.request.Request(full_url, headers=headers)
        response = urllib.request.urlopen(request)
        json_response = json.loads(response.read().decode('utf-8', 'replace'))
        return json_response

    @staticmethod
    def fake_lookup_in_ols(term):
        dict = {
            'power plants':
                {'uri': 'http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#PowerGenerator', 'type': 'class',
                 'from': 'ols'},
            'designed capacity':
                {'uri': 'http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#designCapacity',
                 'type': 'property', 'from': 'ols'},
            'cares lab':
                {'uri': 'http://www.theworldavatar.com/BMS/CARES_Lab.owl#CARES_Laboratory', 'type': 'instance',
                 'from': 'ols'},
            'rooms':
                {'uri': 'http://www.jparksimulator.com/BuildingsLayer.owl#Room', 'type': 'class', 'from': 'ols'}
        }

        return dict.get(term)

    @staticmethod
    def look_up_in_ols(term):
        # print('LookUpService.fake_lookup_in_ols(term)', term, LookUpService.fake_lookup_in_ols(term))
        # return LookUpService.fake_lookup_in_ols(term)

        base_url = 'http://localhost:8080/api/search?q='
        term_to_look = quote('{' + '+'.join([word for word in term.split(' ') if word != 'the']) + '}')
        print('{' + '+'.join(term.split(' ')) + '}')

        url = base_url + term_to_look
        headers = {}
        headers['Accept'] = 'application/json'
        request = urllib.request.Request(url, headers=headers)
        response = urllib.request.urlopen(request)
        json_response = json.loads(response.read().decode('utf-8', 'replace'))
        results = json_response['response']['docs']

        if results:
            return {'type': results[0]['type'].replace('individual', 'instance'), 'uri': results[0]['iri'],
                    'from': 'ols'}
        else:
            return 0

    @staticmethod
    def look_up_in_dbpedia(term):
        url = 'http://lookup.dbpedia.org/api/search/KeywordSearch?QueryClass=&MaxHits=10&QueryString=' + quote(
            term)
        headers = {}
        headers['Accept'] = 'application/json'
        request = urllib.request.Request(url, headers=headers)
        response = urllib.request.urlopen(request)
        json_response = json.loads(response.read().decode('utf-8', 'replace'))
        return json_response

    @staticmethod
    def get_class_definition_in_dbpedia(term):
        json_response = LookUpService.look_up_in_dbpedia(term)
        results = json_response['results']
        classes_list = []
        for result in results:
            if 'classes' in result:
                classes = result['classes']
                for _class in classes:
                    if _class not in classes_list:
                        if 'dbpedia' in _class['uri']:
                            classes_list.append(_class)
        return {'uri': LookUpService.rank_uri_basing_on_label(term, classes_list), 'type': 'class', 'from': 'dbpedia'}

    @staticmethod
    def get_instance_in_dbpedia(term):
        json_response = LookUpService.look_up_in_dbpedia(term)
        results = json_response['results']
        pairs = []
        for result in results:
            pairs.append({'uri': result['uri'], 'label': result['label']})

        return {'uri': LookUpService.rank_uri_basing_on_label(term, pairs), 'type': 'instance', 'from': 'dbpedia'}

    @staticmethod
    def get_predicate_in_dbpedia(term, root_sub, root_sub_type):
        # in order to get the predicate, you need the term and the sub to query with
        if root_sub_type == 'class':
            if type(root_sub) == type({}):
                query = '''PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                           SELECT DISTINCT ?p WHERE { ?s rdf:type <%s> . ?s ?p ?o} ''' % root_sub['uri']
                sparqlEngine = SPARQLEngine()
                result = sparqlEngine.fire_query(query)
                predicates = sparqlEngine.get_predicates_by_query(result)
                return {'uri': LookUpService.rank_uri_basing_on_label(term, predicates), 'type': 'property',
                        'from': 'dbpedia'}
            else:

                query = '''PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                           SELECT DISTINCT ?p WHERE { ?s rdf:type <%s> . ?s ?p ?o}  ''' % root_sub
                sparqlEngine = SPARQLEngine()
                result = sparqlEngine.fire_query(query)
                predicates = sparqlEngine.get_predicates_by_query(result)
                return {'uri': LookUpService.rank_uri_basing_on_label(term, predicates), 'type': 'property',
                        'from': 'dbpedia'}

        elif root_sub_type == 'instance':
            query = '''SELECT DISTINCT ?p WHERE { <%s> ?p ?o} ''' % root_sub
            sparqlEngine = SPARQLEngine()
            result = sparqlEngine.fire_query(query)
            predicates = sparqlEngine.get_predicates_by_query(result)
            return {'uri': LookUpService.rank_uri_basing_on_label(term, predicates), 'type': 'property',
                    'from': 'dbpedia'}

    @staticmethod
    def rank_uri_basing_on_label(term, uri_pairs_list):
        scores = []
        uris = []
        for uri_pair in uri_pairs_list:
            term = ' '.join(re.sub('(?!^)([A-Z][a-z]+)', r' \1', term).split())

            score = compareString.phraseSimilarity(uri_pair['label'], term)
            # if score >= 0.6:
            #     print('\n=================================')
            #     print('==== score ====', score)
            #     print('==== term ====', term)
            #     print('==== label ====', uri_pair['label'])
            #     print('=================================\n')

            scores.append(score)

            uris.append(uri_pair['uri'])
        sorted_uris = [x for _, x in sorted(zip(scores, uris), reverse=True)]
        dbpedia_property_recitifier_dictionary = config.read_file(config.DBPEDIA_PROPERTY_RECTIFIER_DICTIONARY)
        if sorted_uris:
            if sorted_uris[0].strip() in dbpedia_property_recitifier_dictionary.keys():
                sorted_uris[0] = dbpedia_property_recitifier_dictionary[sorted_uris[0]]
            return sorted_uris[0]
