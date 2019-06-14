from NLP_Toolbox import toolbox
from NLQ_Lookup import LookUpService
from NLP_Engine import NLP_Engine
from NLQ_SPARQL import SPARQLEngine


class QueryConstructor:

    def __init__(self):
        pass

    @staticmethod
    def construct_func_header(v, funcs):
        if 'FUNC_SUM' in funcs:
            print('=== sumof' + v)
            return {'header': '(SUM(?%s) as ?%s)' % (v, 'sumof' + v), 'variable': '?sumof' + v}
        elif 'FUNC_CI' in funcs:
            print('=== numberof' + v)
            return {'header': '(COUNT(?%s) as ?%s)' % (v, 'numberof' + v), 'variable': '?numberof' + v}
        else:
            return {'header': None, 'variable': '?' + v}

    @staticmethod
    def construct_standard_nin_query(root_np, next_np):

        # print('=== root_np ===', root_np)
        # print('=== next_np ===', next_np)

        if type(root_np['uri']) == type({}):
            root_np['uri'] = root_np['uri']['uri']
            root_np['from'] = root_np['uri']['from']
            root_np['type'] = root_np['uri']['type']
        if type(next_np['uri']) == type({}):
            next_np['uri'] = next_np['uri']['uri']
            next_np['from'] = next_np['uri']['from']
            next_np['type'] = next_np['uri']['type']
        print('\n\t======================================================')
        print('\troot np type: ', root_np['type'], root_np['from'], root_np['uri'])
        print('\tnext np type: ', next_np['type'], next_np['from'], next_np['uri'])
        print('\t======================================================\n')

        if root_np['type'] == 'instance':
            if next_np['type'] == 'class':
                head = QueryConstructor.construct_func_header(next_np['variable'], next_np['funcs'])
                if head['header']:
                    header = head['header']
                else:
                    header = '?' + next_np['variable']
                query = '''
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                SELECT DISTINCT %s WHERE {  
                ?%s rdf:type <%s> .
                <%s> %s ?%s
            } ''' % (header, next_np['variable'], next_np['uri'], root_np['uri'],
                     SPARQLEngine.construct_random_predicate(), next_np['variable'])

            elif next_np['type'] == 'property':
                head = QueryConstructor.construct_func_header(next_np['variable'], next_np['funcs'])
                if head['header']:
                    header = head['header']
                else:
                    header = '?' + next_np['variable']
                query = '''
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                SELECT DISTINCT %s WHERE {
                    <%s> <%s> ?%s
                }''' % (header, root_np['uri'], next_np['uri'], next_np['variable'])

        elif root_np['type'] == 'class':
            if next_np['type'] == 'class':
                query = '''
                    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                        SELECT DISTINCT ?%s ?%s WHERE {  
                        ?%s rdf:type <%s> .
                        ?%s rdf:type <%s> .
                    } ''' % (root_np['variable'], next_np['variable'], root_np['variable'], root_np['uri'], next_np['variable'], next_np['uri'])


            elif next_np['type'] == 'property':

                query = '''
                     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                         SELECT DISTINCT ?%s ?%s WHERE {  
                         ?%s rdf:type <%s> .
                         ?%s <%s> ?%s  .
                     } ''' % (root_np['variable'], next_np['variable'], root_np['variable'], root_np['uri'], root_np['variable'], next_np['uri'], next_np['variable'])

        return {'query': query, 'from': next_np['from']}

    @staticmethod
    def construct_standard_ninvsnin_query(root_sub, component_1, component_2):

        components = [component_1, component_2]

        service_wrapper = '''                
                
                  SERVICE <http://dbpedia.org/sparql>
                   { %s %s}
                   
                   
                   '''

        normal_wrapper = '''
                {
                
                SELECT DISTINCT %s %s WHERE {  
                    %s
                }
                    %s
                }
        '''

        whole_wrapper = '''
                PREFIX  rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                SELECT DISTINCT  %s %s %s WHERE{
                    %s %s 
                
                }
        '''

        sum_header = '''(SUM(?%s) as ?%s)'''
        count_header = '''(COUNT(?%s) as ?%s)'''
        third_variable = '?' + root_sub['variable']

        root_variable = '?' + root_sub['variable']

        group_by = '''GROUP BY ?%s '''
        if root_sub['type'] == 'class':
            group_by = group_by % (root_sub['variable'])
        else:
            root_variable = ''
            group_by = ''
        queries = []
        headers = []
        group_target = component_1['variable']
        for component in components:
            if component['from'] == 'ols':
                internal_head = ''
                header = QueryConstructor.construct_func_header(component['variable'], component['funcs'])
                if header['header']:
                    headers.append(header['variable'])
                    internal_head = header['header']
                else:
                    headers.append(header['variable'])
                    internal_head = header['variable']

                query = normal_wrapper % (root_variable, internal_head, component['component'], group_by)
                queries.append(query)


            elif component['from'] == 'dbpedia':
                if root_sub['type'] == 'class':
                    root_component =  root_sub['component']
                else:
                    root_component = ''
                query = service_wrapper % (root_component, component['component'])
                queries.insert(0, query)
                header = QueryConstructor.construct_func_header(component['variable'], component['funcs'])
                if header['header']:
                    headers.append(header['variable'])
                else:
                    headers.append(header['variable'])
                group_target = component['variable']

        if root_sub['type'] == 'class':
            pass
            #group_by = group_by % (root_sub['variable'])
        else:
            #group_by = group_by % group_target
            third_variable = ''
        final_query = whole_wrapper % (headers[0], headers[1], third_variable, queries[0], queries[1])
        print('\t ==================================================================')
        print('\t', final_query)
        print('\t ==================================================================')
        return final_query
        #
        # print('\t === root sub type : ==== ', root_sub['type'])
        # print('\t === root sub from : ==== ', root_sub['from'])
        # print('\t === root sub vari : ==== ', root_sub['variable'])
        # print('\t === root sub comp : ==== ', root_sub['component'])
        #
        # print('\t ---------------------------------------------------------')
        # print('\t === comp 1   from : ==== ', component_1['from'])
        # print('\t === comp 1   comp : ==== ', component_1['component'])
        # print('\t === comp 1   vari : ==== ', component_1['variable'])
        # print('\t === comp 1   func : ==== ', component_1['funcs'])
        # print('\t ---------------------------------------------------------')
        # print('\t === comp 2   from : ==== ', component_2['from'])
        # print('\t === comp 2   comp : ==== ', component_2['component'])
        # print('\t === comp 2   vari : ==== ', component_2['variable'])
        # print('\t === comp 2   func : ==== ', component_2['funcs'])
        # print('\t ---------------------------------------------------------')
