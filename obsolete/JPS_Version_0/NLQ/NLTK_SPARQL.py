from SPARQLWrapper import SPARQLWrapper, JSON
import re


class SPARQLEngine:

    def __init__(self):
        self.endpoint_base_url = 'http://dbpedia.org/sparql'
        self.sparql = SPARQLWrapper(self.endpoint_base_url)
        self.template_sub = """ select DISTINCT ?p
                            where {
                            <%s> ?p ?o .
                            } 
                            LIMIT 100"""

        self.template_obj = """ select DISTINCT ?p
                            where {
                             ?s ?p <%s>.
                            } 

                            LIMIT 100"""

    def construct_pre_query_as_sub(self, sub_term):
        template = self.template_sub
        query = (template % sub_term)
        # print('Query:')
        # print(query)
        return query

    def construct_pre_query_as_obj(self, obj_term):
        template = self.template_obj
        query = (template % obj_term)
        # print('Query:')
        # print(query)
        return query

    def fire_query(self, query, sub_term, as_what_pos):
        stop_word_list_for_predicate = ['type', 'subject', 'label', 'comment', 'same As', 'see Also', 'url',
                                        'wiki Page Revision ID',
                                        'wiki Page ID', 'wiki Page Wiki Link', 'wiki Page External Link']
        sparql = self.sparql
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        # print('-------', sub_term, '---------')
        predicate_list = []
        for result in results["results"]["bindings"]:
            value = result['p']['value']
            if '#' in value:
                label = value.split('#')[-1]
                label = ' '.join(re.sub('(?!^)([A-Z][a-z]+)', r' \1', label).split())
            else:
                label = value.split('/')[-1]
                label = ' '.join(re.sub('(?!^)([A-Z][a-z]+)', r' \1', label).split())
            _pair = {'as_what_pos': as_what_pos, 'label': label, 'uri': value}
            if label not in stop_word_list_for_predicate:
                predicate_list.append(_pair)
        return predicate_list
