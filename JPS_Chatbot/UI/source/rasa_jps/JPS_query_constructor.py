import json
import re
import urllib.parse
import urllib.request
from pprint import pprint

from .species_validator import SpeciesValidator
from .attribute_mapping import AttributeMapper


from .locations import JPS_SPARQL_TEMPLATE_PATH
from .search_interface import SearchInterface
from .OntoCompChem_Queries import ontocompchem_simple_intents, \
    ROTATIONAL_CONSTANT_QUERY, VIBRATION_FREQUENCY_QUERY, \
    ROTATIONAL_SYMMETRY_NUMBER, GAUSSIAN_FILE, SPIN_MULTIPLICITY, \
    FORMAL_CHARGE, ELECTRONIC_ENERGY, GEOMETRY_TYPE

from .OntoOntokin_Queries import  GENERAL_QUERY, LENNARD_JONES_WELL_DEPTH, \
    POLARIZABILITY, DIPOLE_MOMENT, RELAXATION_COLLISION,\
    ontokin_simple_intents

from functools import lru_cache
# from cachier import cachier
import datetime
# try:
#     from __main__ import socketio
#
#     print('Importing socketIO from main in interpretation')
# except ImportError:
#     from run import socketio

class JPS_query_constructor:

    def __init__(self, socketio):
        with open(JPS_SPARQL_TEMPLATE_PATH) as f:
            self.template_dict = json.loads(f.read())
        self.serach_interface = SearchInterface()
        self.socketio = socketio
        self.validator = SpeciesValidator()
        self.attribute_mapper = AttributeMapper()
        # self.fire_query.clear_cache()
        # self.fire_query_ontochemcomp.clear_cache()

    @staticmethod
    def process_species_for_ontocompchem(species):
        # to convert H2O2 or h2o2 to H 2 O 2
        # to convert H2O2 or h2o2 to H 2 O 2



        temp = ''
        number_regex = r'[0-9]+'
        alphabet_regex = r'[a-zA-Z]'
        print('-----------------------')
        print('species', species)
        if type(species) == str:

            numbers = re.findall(number_regex, species)
            for number in list(set(numbers)):
                new_number = ' ' + number + ' '
                species = species.replace(number, new_number)

            return species.strip().upper()
            # return result
        else:
            return None

    @staticmethod
    def extract_entity_pairs(entities):
        rst = '<br/>'
        for e in entities:
            key = e['entity']
            value = e['value']
            pair = value + ' - ' + key + '<br/>'
            rst = rst + pair
        print('The entities extracted', rst)
        return rst

    def extract_info(self, intents):

        intent = intents['intent']['name']
        entity_pairs = JPS_query_constructor.extract_entity_pairs(intents['entities'])
        self.socketio.emit('coordinate_agent', 'Looking up entities in JPS KG<br/> -----------------' + str(entity_pairs) + '-----------------')

        if intent in ontocompchem_simple_intents or (intent == 'query_quantum_chemistry'):
            result = {'intent': intent}
            for e in intents['entities']:
                entity_type = e['entity']
                value = e['value']
                if entity_type == 'species':
                    result['species'] = value
                if entity_type == 'attribute':
                    result['attribute'] = value
            return result

        if intent in ontokin_simple_intents or (intent == 'query_thermodynamic'):
            result = {'intent': intent}
            for e in intents['entities']:
                entity_type = e['entity']
                value = e['value']
                if entity_type == 'species':
                    result['species'] = value
                if entity_type == 'attribute':
                    result['attribute'] = value


            return result

        if intent == 'query_reaction_property':
            result = {'intent': intent}
            result['reactants'] = []
            result['products'] = []
            flag = False
            for e in intents['entities']:
                entity_type = e['entity']
                value = e['value']
                if entity_type == 'to':
                    flag = True
                if entity_type == 'attribute':
                    result['attribute'] = value
                if entity_type == 'indicator':
                    result['indicator'] = value
                if entity_type == 'species' and not flag:
                    # this belongs to reactants
                    result['reactants'].append(value.upper())
                if entity_type == 'species' and flag:
                    # this belongs to reactants
                    result['products'].append(value.upper())
            return result
        elif intent == 'select_reaction_by_species':
            result = {'intent': intent}
            result['reactants'] = []
            result['products'] = []
            temp = []
            flag = False
            for e in intents['entities']:
                entity_type = e['entity']
                value = e['value']
                if entity_type == 'indicator':
                    if 'produc' in value:
                        flag = True
                        result['products'] = temp
                        temp = []
                    else:
                        result['reactants'] = temp
                        temp = []

                if entity_type == 'species':
                    # this belongs to reactants
                    temp.append(value.upper())
            if (not result['reactants']) and (not result['products']):
                result['products'] = temp
            print('-------------- result processed -------------', result)
            return result
        elif intent == 'select_mechanism_by_reaction':
            result = {'intent': intent}
            result['reactants'] = []
            result['products'] = []
            flag = False
            for e in intents['entities']:
                entity_type = e['entity']
                value = e['value']
                if entity_type == 'to':
                    flag = True
                if entity_type == 'attribute':
                    result['attribute'] = value
                if entity_type == 'species' and not flag:
                    # this belongs to reactants
                    result['reactants'].append(value.upper())
                if entity_type == 'species' and flag:
                    # this belongs to reactants
                    result['products'].append(value.upper())
            print('========= select_mechanism_by_reaction ===========')
            pprint(intents)
            return result

    def construct_query(self, intents):
        self.socketio.emit('coordinate_agent', 'Constructing SPARQL queries')
        print('=================== intents ================')
        pprint(intents)
        result = self.extract_info(intents)
        intent = result['intent']
        print('----- from construct query -------')
        print(intent)
        print(intent in ontokin_simple_intents)

        print('----------------------------------')

        if intent == 'query_reaction_property':
            try:
                rst = self.query_reaction_property(result['reactants'], result['products'], result['attribute'])
            except:
                rst = self.query_reaction_property(result['reactants'], result['products'], result['indicator'])
        elif intent == 'select_reaction_by_species':
            # TODO: seperate reactants and products
            print('select_reaction_by_species')
            rst = self.query_by_reaction_only(result['reactants'], result['products'])
        elif intent == 'select_mechanism_by_reaction':
            rst = self.query_mechanism_by_reaction(result['reactants'], result['products'])
            if rst is None:
                return None
        elif intent in ontocompchem_simple_intents or (intent == 'query_quantum_chemistry'):
            rst = self.query_quantum_of_moleculars(result['intent'], result['species'], result['attribute'])
            if rst is None:
                return None
        elif intent in ontokin_simple_intents or (intent == 'query_thermodynamic'):
            rst = self.query_thermo_of_moleculars(result['intent'], result['species'], result['attribute'])
            if rst is None:
                return None


        return rst.replace('[=]', '->').replace('=]', '->')

    def query_thermo_of_moleculars(self, intent, species, attribute):

        print('=========== species received ===========')
        print('species:', species)
        print('=========== attribute received ============')
        print('attribute:', attribute)
        species = species.upper()
        attribute_iri = self.attribute_mapper.find_closest_attribute(intent, attribute)
        print('=========== attribute iri  ============')
        print('attribute iri :', attribute_iri)
        print('============= line 206 ============')
        print('intent', intent)
        print('species', species)
        species = self.validator.validate(attribute, 'ontokin', intent, species)
        print('======== species =======')
        print(species)

        if intent == 'query_thermodynamic':
            q = GENERAL_QUERY % (attribute.replace(' ', '').upper(), species, attribute_iri, attribute.replace(' ', '').upper(), attribute_iri)
            print('================ GENERAL QUERY ===============')
            print(q)
            rst = self.fire_query(q).decode('utf-8')
            return rst
        # # 1. att name, 1.5 species  2. att iri name 3. att name 4. att iri name
        elif intent == 'lennard_jones_well':
            q = LENNARD_JONES_WELL_DEPTH % species
            print('===========JONE WELL DEPTH========== ')
            print(q)
            print('=======================')
            rst = self.fire_query(q).decode('utf-8')
            return rst
        elif intent == 'polarizability':
            q = POLARIZABILITY % species
            rst = self.fire_query(q).decode('utf-8')

        elif intent == 'dipole_moment':
            q = DIPOLE_MOMENT % species
            rst = self.fire_query(q).decode('utf-8')

        elif intent == 'rotational_relaxation_collision':
            q = RELAXATION_COLLISION % species
            rst = self.fire_query(q).decode('utf-8')

        else:
            return None

        if rst is None:
            return None
        else:
            rst = json.loads(rst)
            rst = json.dumps(rst)
            print('result from ontokin', rst)
            return rst

    def query_quantum_of_moleculars(self, intent, species, attribute):
        # ROTATIONAL_CONSTANT_QUERY
        # VIBRATION_FREQUENCY_QUERY
        # ROTATIONAL_SYMMETRY_NUMBER
        original_species = species
        print('=========== line 238 =============')
        print('intent', intent)
        print('species', species)
        attribute_iri = self.attribute_mapper.find_closest_attribute(intent, attribute)
        species = self.validator.validate(attribute, 'ontocompchem', intent, species)
        intent = self.attribute_mapper.map_to_quantum_queries(attribute_iri)
        self.socketio.emit('coordinate_agent', 'this is from the validator' + str(species))
        if species is None:
            self.socketio.emit('coordinate_agent', 'This species does not have this information in the World Avatar KG')
            return None

        if intent == 'rotational_constants':
            q = ROTATIONAL_CONSTANT_QUERY % species
            rst = self.fire_query_ontochemcomp(q).decode('utf-8')
        elif intent == 'symmetry_number':
            q = ROTATIONAL_SYMMETRY_NUMBER % species
            rst = self.fire_query_ontochemcomp(q).decode('utf-8')
        elif intent == 'vibration_frequency':
            q = VIBRATION_FREQUENCY_QUERY % species
            rst = self.fire_query_ontochemcomp(q).decode('utf-8')
        elif intent == 'guassian_file':
            q = GAUSSIAN_FILE % species
            rst = self.fire_query_ontochemcomp(q).decode('utf-8')
        elif intent == 'spin_multiplicity':
            q = SPIN_MULTIPLICITY % species
            rst = self.fire_query_ontochemcomp(q).decode('utf-8')
        elif intent == 'formal_charge':
            q = FORMAL_CHARGE % species
            rst = self.fire_query_ontochemcomp(q).decode('utf-8')
        elif intent == 'electronic_energy':
            q = ELECTRONIC_ENERGY % species
            rst = self.fire_query_ontochemcomp(q).decode('utf-8')
        elif intent == 'geometry_type':
            q = GEOMETRY_TYPE % species
            rst = self.fire_query_ontochemcomp(q).decode('utf-8')


        else:
            return None
        if rst is None:
            return None
        else:
            rst = self.process_ontocompchem_results(rst)
            print('result from ontocompchem', rst)
            return rst

    def process_ontocompchem_results(self, rst):

        rst_lines = rst.split('\r\n')
        print(rst_lines)

        if len(rst_lines) <= 1:
            return None
        else:
            result = []
            heads = rst_lines[0].split(',')
            data_list = rst_lines[1:]
            print('------ data list --------', data_list)
            for data in data_list:
                temp = {}
                cols = data.split(',')
                for c, h in zip(cols, heads):
                    temp[h] = c
                if data == '':
                    pass
                else:
                    result.append(temp)
            print('------- processed ontocompchem data ---------')
            print(result)
            return json.dumps(result)

    def query_mechanism_by_reaction(self, reactants, products):
        print('query_mechanism_by_reaction')
        q = self.construct_query_reaction_by_species(reactants, products)
        if q is None:
            return None
        q = q % ('?MechanismName', '''\n ?reaction ontokin:belongsToPhase ?Phase .
?Phase ontokin:containedIn ?MechanismIRI .
?MechanismIRI rdfs:label ?MechanismName .''')
        rst = self.fire_query(q).decode('utf-8')
        rst = self.add_comma(rst)
        return rst

    def query_by_reaction_only(self, reactants, products):
        print('query_by_reaction_only')
        q = self.construct_query_reaction_by_species(reactants, products) % ('', '')
        print(q)
        rst = self.fire_query(q).decode('utf-8')
        rst = self.add_comma(rst)
        return rst

        # TODO: construct the query by only reactants and products

    def query_reaction_property(self, reactants, products, attribute):
        print('query_reaction_property')

        sub_properties = []
        sub_properties_arrhenius = ['ontokin:hasActivationEnergy', 'ontokin:hasActivationEnergyUnits ',
                                    'ontokin:hasPreExponentialFactor', 'ontokin:hasPreExponentialFactorUnits',
                                    'ontokin:hasTemperatureExponent', 'ontokin:hasTemperatureExponentUnits']

        sub_properties_products = ['rdfs:label']

        attribute = ' <' + self.serach_interface.get_first_match(attribute).strip() + '> '
        print('============== attribute =============')
        print(attribute)
        new_labels = ''

        propertyName = ''
        if 'hasArrheniusCoefficient' in attribute:
            sub_properties = sub_properties_arrhenius
            propertyName = 'ArrheniusCoefficient'
        elif 'isReversible' in attribute:
            propertyName = 'isReversible'
            sub_properties = []
            new_labels = ' ?%s ' % propertyName
        elif 'hasProduct' in attribute:
            propertyName = 'hasProduct'
            sub_properties = sub_properties_products
            new_labels = ' ?%s ' % propertyName

        sub_query = ''
        for num, p in enumerate(sub_properties, start=1):
            l = p.replace('ontokin:has', '')
            query_line = '?%s ' % propertyName + p + ' ?' + l.replace('rdfs:label', 'label') + ' .\n '
            sub_query = sub_query + query_line
            new_labels = new_labels + ' ?' + l.replace('rdfs:label', 'label')
        print('=============== new labels ==============')
        print(new_labels)
        print('-------------- sub_query -------------')
        print(sub_query)

        q = self.construct_query_reaction_by_species(reactants, products) % (
            new_labels, '\n{\n ?reaction %s ?%s . \n %s  }' % (attribute, propertyName, sub_query))
        q = q.replace('DISTINCT', '') + ' LIMIT 1'
        print(q)

        rst = self.fire_query(q).decode('utf-8')
        rst = self.add_comma(rst)
        return rst

    def add_comma(self, result):
        print('result before', result)
        result = result.replace('\r', '').replace('\n', '')

        result = result.replace('}{', '},{')
        result = re.sub(r'''}[ ]+{''', '},{', result)
        print('result after', result)
        return result

    # @lru_cache(maxsize=64)
    @lru_cache(maxsize=None)
    def fire_query(self, query):

        print('Importing socketIO from run_socket in interpretation')
        self.socketio.emit('coordinate_agent', 'Querying the Ontokin ontology in the JPS Knowledge Graph')

        print('----------- firing the query to JPS -------------')
        print(query)
        # x = input()
        url = "http://www.theworldavatar.com/OntoKinGUI/OntoKinEndpointProxy"
        values = {'queryString': query}
        data = urllib.parse.urlencode(values).encode('utf-8')
        print(type(data))
        req = urllib.request.Request(url, data)
        response = urllib.request.urlopen(req).read()
        return response

    @lru_cache(maxsize=None)
    def fire_query_ontochemcomp(self, query):
        print('----------- firing the query to JPS ontochemcomp -------------')
        print(query)
        self.socketio.emit('coordinate_agent', 'Querying the OntoCompChem ontology in the JPS Knowledge Graph')

        # x = input()
        url = "http://www.theworldavatar.com/rdf4j-server/repositories/ontocompchem"
        values = {'query': query}
        data = urllib.parse.urlencode(values).encode('utf-8')
        print(type(data))
        req = urllib.request.Request(url, data)
        response = urllib.request.urlopen(req).read()
        return response

    def construct_query_reaction_by_species(self, reactants, products):
        print('--------- reactants -------------')
        print(reactants)
        print('--------- products -------------')
        print(products)

        if len(reactants) == 0 and len(products) == 0:
            return None
        query_reaction_by_species_template = self.template_dict['select_reaction_by_species']
        reactants_num = len(reactants)
        reactant_part = ''
        products_num = len(products)
        products_part = ''
        headers = ''
        label_part = ''
        regex_part = ''
        # put reactants first
        for num, name in enumerate(reactants, start=1):
            v = 'Reactant_%s' % str(num)
            headers = headers + ' ?' + v
            if num == reactants_num and products_num == 0:
                line = 'reaction:hasReactant ?%s .' % v
            else:
                line = 'reaction:hasReactant ?%s ;' % v
            reactant_part = reactant_part + '\n' + line
            l = '?%s rdfs:label ?%s_label . ' % (v, v)
            label_part = label_part + '\n' + l
            r = 'FILTER regex(?%s_label, "^%s$")' % (v, name)
            regex_part = regex_part + '\n' + r

        for num, name in enumerate(products, start=1):
            v = 'Product_%s' % str(num)
            headers = headers + ' ?' + v
            if num == products_num:
                line = 'reaction:hasProduct ?%s .' % v
            else:
                line = 'reaction:hasProduct ?%s ;' % v
            products_part = products_part + '\n' + line
            l = '?%s rdfs:label ?%s_label . ' % (v, v)
            label_part = label_part + '\n' + l
            r = 'FILTER regex(?%s_label, "^%s$")' % (v, name)
            regex_part = regex_part + '\n' + r

        headers = '%s'
        if reactants_num != 0:
            q = query_reaction_by_species_template % (
                headers, reactant_part + products_part, label_part, regex_part, '%s')
        else:
            q = query_reaction_by_species_template % (headers, products_part, label_part, regex_part, '%s')

        return q
