from SPARQLWrapper import SPARQLWrapper, JSON
import re
import random
import urllib.request
from urllib.parse import quote
import json


class SPARQLEngine:
    # http://www.theworldavatar.com/damecoolquestion/nlp/query
    def __init__(self):
        self.endpoint_base_url = 'http://dbpedia.org/sparql'
        self.sparql = SPARQLWrapper(self.endpoint_base_url)

    def fire_mix_query(self, query):
        print('query: ', query)
        return SPARQLEngine.general_url_request('http://www.theworldavatar.com/damecoolquestion/nlp?&output=json&query=' + quote(query))

    def fire_query(self, query):

        sparql = self.sparql
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return results

    def get_predicates_by_query(self, results):

        stop_word_list_for_predicate = ['type', 'subject', 'label', 'comment', 'same As', 'see Also', 'url',
                                        'wiki Page Revision ID',
                                        'wiki Page ID', 'wiki Page Wiki Link', 'wiki Page External Link']
        predicate_list = []
        for result in results["results"]["bindings"]:
            value = result['p']['value']
            if '#' in value:
                label = value.split('#')[-1]
                label = ' '.join(re.sub('(?!^)([A-Z][a-z]+)', r' \1', label).split())
            else:
                label = value.split('/')[-1]
                label = ' '.join(re.sub('(?!^)([A-Z][a-z]+)', r' \1', label).split())
            pair = {'label': label, 'uri': value}
            if label not in stop_word_list_for_predicate:
                predicate_list.append(pair)
        return predicate_list

    @staticmethod
    def check_use_as_sub_or_obj(candidate_for_sub, candidate_for_obj):

        # 1 construct the query for sub - unknown - obj

        template_1 = '''         
        PREFIX rdf: <rdf>
        SELECT ?p WHERE {
            %s
            %s
            ?%s ?p ?%s
        }'''

        template_2 = '''
        PREFIX rdf: <rdf>
        SELECT ?p WHERE {
            %s 
            %s %s
        }'''

        if candidate_for_sub['type'] == 'class':
            query_1 = template_1 % (candidate_for_sub['component'], candidate_for_obj['component'], candidate_for_sub['variable'], candidate_for_obj['variable'])
            query_2 = template_1 % (candidate_for_sub['component'], candidate_for_obj['component'], candidate_for_obj['variable'], candidate_for_sub['variable'])
            query = '''\t %s \n ?%s %s ?%s .''' % (candidate_for_obj['component'],
                                                   candidate_for_obj['variable'],
                                                   SPARQLEngine.construct_random_predicate(),
                                                   candidate_for_sub['variable'])

        elif candidate_for_sub['type'] == 'instance':
            query_1 = template_2 % (candidate_for_obj['component'], candidate_for_obj['variable'], candidate_for_sub['component'])
            query_2 = template_2 % (candidate_for_obj['component'], candidate_for_sub['component'], candidate_for_obj['variable'])
            query = '''\t %s \n ?%s %s %s . ''' % (candidate_for_obj['component'],
                                                  candidate_for_obj['variable'],
                                                  SPARQLEngine.construct_random_predicate(),
                                                  candidate_for_sub['component'])

        return query

    @staticmethod
    def construct_random_predicate():
        return '?p' + str(random.randint(1, 100))

    @staticmethod
    def construct_get_numerical_value_component(variable_name):
        component_get_numerical_value = '''               
               ?%s <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue> ?value%s .
               ?value%s <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> ?nvOf%s
               '''
        random_number = str(random.randint(1, 100))
        component = component_get_numerical_value % (variable_name, random_number, random_number, variable_name)
        return {'component': component, 'variable': 'nvOf' + variable_name}

    @staticmethod
    def construct_variable_name(term):
        return term.replace(' ', '').strip()

    @staticmethod
    def general_url_request(full_url):
        """Simply how to make a url request in python, giving the full quoted requets url -> response in json format"""
        headers = {}
        headers['Accept'] = 'application/json'
        request = urllib.request.Request(full_url, headers=headers)
        response = urllib.request.urlopen(request)
        json_response = json.loads(response.read().decode('utf-8', 'replace'))
        return json_response
