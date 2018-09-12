from NLP_Toolbox import toolbox
from NLQ_Lookup import LookUpService
from NLP_Engine import NLP_Engine
from NLQ_SPARQL import SPARQLEngine
from NLQ_QueryConstructor import QueryConstructor
import pprint


class PatternProcessor:
    #  ________________________________________________________________________________
    #  ||| What are the value of temperature sensor in open area in cares lab |||
    #  ________________________________________________________________________________
    #   SELECT DISTINCT * WHERE {
    #       <CARES_LAB>     ?random     <open area> .
    #       ?temperatureSensor        type        <temperature sensor> .
    #       <open area>   ?random2    ?temperatureSensor .
    #       ?temperatureSensor        hasValue    ?value
    #   }
    # ______________________________________________________________________________________________
    # ||| What is the sum of designed capacity of power plants versus the number of power plants of all country
    # ______________________________________________________________________________________________
    #
    #  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    #    SELECT DISTINCT ?country (SUM(?nv) as ?tt) (COUNT(?plant) as ?npp)  WHERE {
    #       ?country rdf:type <http://dbpedia.org/ontology/Country> . # get all the countries
    #       ?plant rdf:type <http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#PowerGenerator> . #
    #       ?plant ?unknownP ?county .
    #       ?plant <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#designCapacity> ?capacity .
    #       ?capacity <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue> ?value .
    #       ?value <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> ?nv
    #   }  GROUP BY ?country ORDER BY DESC(?tt)

    def __init__(self):
        self.function_map = {'NINCHAINCHAIN': self.NINCHAINCHAIN,
                             'NINVSNIN': self.NINVSNIN,
                             'NINCHAIN': self.NINCHAIN,
                             'NIN': self.NIN,
                             'SINGLENP': self.SINGLENP}

        self.main_header = '''PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                              SELECT DISTINCT ?%s ?%s  WHERE '''

        self.query_wrapper = '{ } %s'  # GROUP BY ?root_sub

    def SINGLENP(self, tree):
        print('Entering SINGLENP')
        tree = toolbox.get_root_sub(tree)
        np = toolbox.np_processor(tree)
        result = PatternProcessor.get_uri_for_root_np(np)
        np['uri'] = result
        print(np)

    def NIN(self, tree):
        print('Entering NIN')

        # ========================= Stage 1 =============================
        tree = toolbox.remove_qi(tree)
        root_np_tree = PatternProcessor.get_root_sub(tree)
        root_np = toolbox.np_processor(root_np_tree)
        if 'FUNC_ALL' in root_np['funcs']:
            root_np.update(LookUpService.get_sub_class_uri(root_np['term']))
        else:
            root_np.update(LookUpService.get_sub_instance_uri(root_np['term']))
        root_np['variable'] = SPARQLEngine.construct_variable_name(root_np['term'])
        root_np['component'] = PatternProcessor.construct_components(root_np, root_np['term'])
        print(root_np['component'])
        # ========================= Stage 2 =============================
        tree = toolbox.get_the_only_sub_tree(tree)
        tree.remove(root_np_tree)
        next_np = toolbox.get_the_only_sub_tree(tree)
        next_np = toolbox.np_processor(next_np)
        print('next_np ', next_np)

        print('next np funcs', next_np['funcs'])
        if 'FUNC_ALL' in next_np['funcs']:
            uri = LookUpService.get_sub_class_uri(next_np['term'])
            next_np['type'] = 'class'
            next_np.update(uri)
        else:
            uri = LookUpService.get_predicate_uri(next_np['term'], root_np['uri'], root_np['type'])
            next_np.update(uri)

        print('next np', next_np)
        next_np['variable'] = SPARQLEngine.construct_variable_name(next_np['term'])

        query_result = QueryConstructor.construct_standard_nin_query(root_np, next_np)
        query = query_result['query']
        domain = query_result['from']

        sparqlEngine = SPARQLEngine()
        print('========= query =========')
        print(query)
        print('=========================')
        if domain == 'ols':
            print('=================== Searching ols ===================')
            result = sparqlEngine.fire_mix_query(query)
            return result
        else:
            print('=================== Searching dbpedia ===================')
            result = sparqlEngine.fire_query(query)
            return result

    def NINCHAINCHAIN(self, tree):
        print('Entering NINCHAINCHAIN')
        tree = toolbox.remove_qi(tree)

    def NINCHAIN(self, tree):
        print('Entering NINCHAIN')
        tree = toolbox.remove_qi(tree)
        root_tree = PatternProcessor.get_root_sub_and_search_for_uri(tree)
        root_np = {}
        root_np.update(root_tree)
        root_np['component'] = PatternProcessor.construct_components(root_np, root_np['variable'])

        NPs = [np for np in root_np['tree'] if toolbox.is_tree(np)]
        nin = NPs.pop()
        nin_np = self.nin_processor(nin, root_np)

        print('nin np ', nin_np['component'])

    def NINVSNIN(self, tree):
        print('Entering NINVSNIN')
        tree = toolbox.remove_qi(tree)
        root_tree = PatternProcessor.get_root_sub_and_search_for_uri(tree)
        root_np = {}
        root_np.update(root_tree)
        root_np['component'] = PatternProcessor.construct_components(root_np, root_np['variable'])
        NPs = [np for np in root_np['tree'] if toolbox.is_tree(np)]
        components = []
        for np in NPs:
            np_label = np.label()
            if np_label == 'NIN':
                print(' === Entering NIN processor ===')
                components.append(self.nin_processor(np, root_np))
            else:
                print(' === Entering SINGLENP processor ===')
                components.append(self.singlenp_processor(np, root_np))

        sparqlEngine = SPARQLEngine()
        query_result = sparqlEngine.fire_mix_query(
            QueryConstructor.construct_standard_ninvsnin_query(root_np, components[0], components[1]))
        return query_result

    def singlenp_processor(self, np, root_sub):
        predicate_np_result = PatternProcessor.general_get_root_sub(np)
        predicate_np = predicate_np_result['np']
        funcs = [word[1] for word in np.leaves() if word[1].startswith('FUNC')]
        # ================================================
        predicate_np['uri'] = LookUpService.get_predicate_uri(predicate_np['term'], root_sub['uri'], root_sub['type'])
        predicate_np['type'] = predicate_np['uri']['type']
        predicate_np['variable'] = SPARQLEngine.construct_variable_name(predicate_np['term'])
        predicate_np['funcs'] = funcs
        predicate_np['component'] = PatternProcessor.construct_components(predicate_np['uri'], predicate_np['term'])
        predicate_np['from'] = predicate_np['uri']['from']

        if type(predicate_np['uri']) == type({}):
            predicate_np['uri'] = predicate_np['uri']['uri']
        if predicate_np['type'] == 'class':
            # construct ?plant rdf:type <http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#PowerGenerator> . #
            connection_query_component = PatternProcessor.construct_connect_to_sub(root_sub, predicate_np)
        elif predicate_np['type'] == 'property':
            # constrcut <http://dbpedia.org/resource/Argentina> <http://dbpedia.org/Ontology/gdp_ppp> ?gdp .
            if predicate_np['from'] == 'ols':
                connection_query_component = PatternProcessor.construct_simple_spo_ols(root_sub, predicate_np['uri'],
                                                                                       predicate_np['variable'])
            elif predicate_np['from'] == 'dbpedia':
                connection_query_component = PatternProcessor.construct_simple_spo_service(root_sub,
                                                                                           predicate_np['uri'],
                                                                                           predicate_np['variable'])

        return {'component': connection_query_component, 'variable': predicate_np['variable'],
                'funcs': predicate_np['funcs'], 'from': predicate_np['from']}

        # print('\n\t========================= component =====================')
        # print('\t || ', predicate_np['funcs'])
        # print('\t || ', predicate_np['variable'])
        # print('\t', connection_query_component)
        # print('\t==========================================================\n')

    def nin_processor(self, nin, root_sub):

        # root_sub {'component': root_query_component, 'variable': root_element['variable'], 'type': root_type}
        # 1. get the root sub within nin , name it np_content
        whole_query_component = ''
        # ================================= Stage 1 ====================================

        result = PatternProcessor.general_get_root_sub(nin)
        nin_root_np = result['np']
        nin_root_np['uri'] = PatternProcessor.get_uri_for_root_np(nin_root_np)
        nin_root_np['component'] = PatternProcessor.construct_components(nin_root_np['uri'], nin_root_np['term'])
        nin_root_np['variable'] = SPARQLEngine.construct_variable_name(nin_root_np['term'])
        connection_query_component = PatternProcessor.construct_connect_to_sub(root_sub, nin_root_np)

        # print('\n\t===============Query Component nin 1 ==================')
        # print('\t', connection_query_component)
        # print('\t=================================================')
        # ================================= Stage 2 ====================================
        # use the rest of nps to move on
        next_np_tree = result['new_tree'].pop()
        result = PatternProcessor.general_get_root_sub(next_np_tree)
        next_np = result['np']
        next_np['tree'] = next_np_tree
        next_np['funcs'] = PatternProcessor.get_funcs(next_np['tree'])
        next_np['variable'] = SPARQLEngine.construct_variable_name(next_np['term'])
        next_np.update(PatternProcessor.get_uri_for_next_np(next_np, nin_root_np))
        next_np_query_component = PatternProcessor.construct_simple_spo(nin_root_np['variable'], next_np['uri'],
                                                                        next_np['variable'], next_np['funcs'])









        contains_nv_of = ('nvOf' in next_np_query_component)
        if contains_nv_of:
            next_np['variable'] = 'nvOf' + next_np['variable']

        # print('\n\t===============Query Component nin 2==================')
        # contains_nv_of = ('nvOf' in next_np_query_component)
        # if contains_nv_of:
        #     next_np['variable'] = 'nvOf' + next_np['variable']
        # print('\t || ', next_np['funcs'])
        # print('\t || ', next_np['variable'])
        # print('\t', next_np_query_component)
        # print('\t=================================================')
        return {'component': connection_query_component + '\n' + next_np_query_component,
                'variable': next_np['variable'], 'funcs': next_np['funcs'], 'from': next_np['from']}

    @staticmethod
    def construct_simple_spo(sub_variable, p_uri, obj_variable, next_np_funcs):
        if type(p_uri) == type({}):
            p_uri = p_uri['uri']
        component_template = '''
            ?%s <%s> ?%s .
            %s'''
        nv_component_result = SPARQLEngine.construct_get_numerical_value_component(obj_variable)
        nv_variable = nv_component_result['variable']

        nv_component = nv_component_result['component']
        spo_component = component_template % (sub_variable, p_uri, obj_variable, nv_component)
        return spo_component

    @staticmethod
    def construct_simple_spo_service(sub_element, p_uri, obj_variable):
        if type(p_uri) == type({}):
            p_uri = p_uri['uri']
        sub_element_type = sub_element['type']
        if sub_element_type == 'class':
            component_template = '''?%s <%s> ?%s  ''' % (sub_element['variable'], p_uri, obj_variable)
            return component_template
        elif sub_element_type == 'instance':

            component_template = '''<%s> <%s> ?%s ''' % (sub_element['uri'], p_uri, obj_variable)
            return component_template

    @staticmethod
    def construct_simple_spo_ols(sub_element, p_uri, obj_variable):

        if type(p_uri) == type({}):
            p_uri = p_uri['uri']
        sub_element_type = sub_element['type']
        if sub_element_type == 'class':
            component_template = ''' ?%s <%s> ?%s   .''' % (sub_element['variable'], p_uri, obj_variable)
            return component_template
        elif sub_element_type == 'instance':
            component_template = '''  <%s> <%s> ?%s  .''' % (sub_element['uri'], p_uri, obj_variable)
            return component_template

    @staticmethod
    def construct_components(uri, term):
        variable_name = SPARQLEngine.construct_variable_name(term)
        type = uri['type']
        uri = uri['uri']
        if type == 'class':
            return '''?%s rdf:type <%s> .''' % (variable_name, uri)
        elif type == 'instance':
            return '''<%s>''' % uri

    @staticmethod
    def construct_connect_to_sub(root_element, predicate_element):
        connetion = SPARQLEngine.check_use_as_sub_or_obj(root_element, predicate_element)
        return connetion

    @staticmethod
    def update_header():
        pass

    @staticmethod
    def get_root_sub_and_search_for_uri(tree):
        root_np = PatternProcessor.get_root_sub(tree)
        tree = toolbox.get_the_only_sub_tree(tree)
        tree.remove(root_np)
        tree = NLP_Engine.nin_pattern_recognizer(tree)
        np_result = toolbox.np_processor(root_np)
        term = np_result['term']
        funcs = np_result['funcs']

        variable_name = SPARQLEngine.construct_variable_name(term)

        print('funcs', funcs)
        if 'FUNC_ALL' in funcs:
            uri = LookUpService.get_sub_class_uri(term)
            type = 'class'
        else:
            uri = LookUpService.get_sub_instance_uri(term)
            type = 'instance'

        uri['tree'] = tree
        uri['variable'] = variable_name

        return uri

    @staticmethod
    def get_root_sub(tree):
        NPs = [sub_tree for sub_tree in toolbox.get_the_only_sub_tree(tree) if toolbox.is_tree(sub_tree)]
        if NPs:
            root_np = NPs.pop()
            return root_np
        else:
            return toolbox.np_processor(tree)

    @staticmethod
    def general_get_root_sub(tree):
        NPs = [sub_tree for sub_tree in tree if toolbox.is_tree(sub_tree)]
        if NPs:
            root_np = NPs.pop()
            return {'np': toolbox.np_processor(root_np), 'tree': root_np, 'new_tree': NPs}
        else:
            return {'np': toolbox.np_processor(tree), 'tree': tree, 'new_tree': NPs}

    @staticmethod
    def get_uri_for_next_np(np, nin_root_np):
        if 'FUNC_ALL' in np:
            uri = LookUpService.get_sub_class_uri(np['term'])
        else:
            uri = LookUpService.get_predicate_uri(np['term'], nin_root_np['uri'], 'property')
        print('=== uri ===', uri)
        return uri

    @staticmethod
    def get_funcs(np):
        return [word[1] for word in np.leaves() if word[1].startswith('FUNC')]

    @staticmethod
    def get_uri_for_root_np(np):

        if 'FUNC_ALL' in np['funcs']:
            uri = LookUpService.get_sub_class_uri(np['term'])  #
        else:
            uri = LookUpService.get_sub_instance_uri(np['term'])
        return uri
