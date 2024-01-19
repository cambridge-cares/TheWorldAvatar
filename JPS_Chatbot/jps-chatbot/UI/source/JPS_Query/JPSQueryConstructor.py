import json
import os
import re
import urllib.parse
import urllib.request
from pprint import pprint

from .species_validator import SpeciesValidator
from .attribute_mapping import AttributeMapper
from .locations import JPS_SPARQL_TEMPLATE_PATH, CONFIG_PATH
from .search_interface import SearchInterface
from .OntoCompChem_Queries import ontocompchem_simple_intents, intent_to_template_mapping, ELECTRONIC_ENERGY
from .OntoOntokin_Queries import RELAXATION_COLLISION, \
    ontokin_simple_intents, HIGH_SPEED_GENERAL_QUERY
import hashlib


def process_ontocompchem_results(rst):
    rst_lines = rst.split('\r\n')
    if len(rst_lines) <= 1:
        return None
    else:
        result = []
        heads = rst_lines[0].split(',')
        data_list = rst_lines[1:]
        for data in data_list:
            temp = {}
            cols = data.split(',')
            for c, h in zip(cols, heads):
                temp[h] = c
            if data == '':
                pass
            else:
                result.append(temp)
        return json.dumps(result)


class JPSQueryConstructor:

    def __init__(self, socketio):
        with open(JPS_SPARQL_TEMPLATE_PATH) as f:
            self.template_dict = json.loads(f.read())
        if os.path.isfile(CONFIG_PATH):

            with open(CONFIG_PATH) as f:
                self.config = json.loads(f.read())
        else:
            # By default, assume the LDF server is accessible via the network name "jps-ldf",
            # which should be the case when its running alongside the chatbot in the same docker stack
            self.config = {'ldf_host': 'http://jps-ldf', 'ldf_port': 3000}

        self.serach_interface = SearchInterface()
        self.socketio = socketio
        self.validator = SpeciesValidator()
        self.attribute_mapper = AttributeMapper()

    # Builds the base URL for all LDF queries

    def build_base_url(self):
        # Get the URL listed in the config
        host = str(self.config['ldf_host'])
        # Get the port listed in the config
        port = int(str(self.config['ldf_port']))

        if port > 0:
            url = "%s:%s/" % (host, port)
        else:
            url = "%s/" % host

        return url

    def fire_query_to_ldf_ontocompchem(self, query):
        # Get the base URL
        url = self.build_base_url()
        url += "query?"
        values = {"query": query, "ontology": "ontocompchem"}
        parameter_hash = hashlib.md5(json.dumps(values).encode('utf-8')).hexdigest()
        values['hash'] = parameter_hash
        full_url = url + urllib.parse.urlencode(values)
        req = urllib.request.Request(full_url)
        return urllib.request.urlopen(req).read()

    def fire_query_to_ldf_ontokin(self, query, products, reactants):
        if products is None:
            products = []
        if reactants is None:
            reactants = []

        # Get the base URL
        url = self.build_base_url()
        url += "query?"

        values = {"query": query, "products": json.dumps(products), "reactants": json.dumps(reactants),
                  "ontology": "ontokin"}
        parameter_hash = hashlib.md5(json.dumps(values).encode('utf-8')).hexdigest()
        values['hash'] = parameter_hash
        full_url = url + urllib.parse.urlencode(values)

        req = urllib.request.Request(full_url)
        response = urllib.request.urlopen(req).read()
        return response

    @staticmethod
    def process_species_for_ontocompchem(species):
        # to convert H2O2 or h2o2 to H 2 O 2
        # to convert H2O2 or h2o2 to H 2 O 2
        number_regex = r'[0-9]+'
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
        return rst

    def extract_info(self, intents):
        intent = intents['intent']['name']
        # entity_pairs = JPSQueryConstructor.extract_entity_pairs(intents['entities'])
        # self.socketio.emit('coordinate_agent', 'Looking up entities in JPS KG<br/> -----------------' + str(
        #     entity_pairs) + '-----------------')

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
            result = {'intent': intent, 'reactants': [], 'products': []}
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
            result = {'intent': intent, 'reactants': [], 'products': []}
            temp = []
            for e in intents['entities']:
                entity_type = e['entity']
                value = e['value']
                if entity_type == 'indicator':
                    if 'produc' in value:
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
            return result
        elif intent == 'select_mechanism_by_reaction':
            result = {'intent': intent, 'reactants': [], 'products': []}
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
            return result

    # This is the main interface for the query constructor
    # It handles ontokin and ontocompchem queries.
    def construct_query(self, intents):
        result = self.extract_info(intents)
        intent = result['intent']
        rst = None
        if intent == 'query_reaction_property':
            try:
                rst = self.query_reaction_property(result['reactants'], result['products'], result['attribute'])
            except:
                rst = self.query_reaction_property(result['reactants'], result['products'], result['indicator'])
        elif intent == 'select_reaction_by_species':
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

        species = species.upper()
        attribute_iri = self.attribute_mapper.find_closest_attribute(intent, attribute)
        # the species validator transform species recieved into a proper form
        species = self.validator.validate(attribute, 'ontokin', intent, species)

        if intent == 'query_thermodynamic':
            attribute_name = attribute.replace(' ', '').upper()
            q = HIGH_SPEED_GENERAL_QUERY % (attribute_name, attribute_iri, attribute_name, species, attribute_iri)
            rst = self.fire_query_to_ldf_ontokin(q, None, None).decode('utf-8')
            return rst
        # # 1. att name, 1.5 species  2. att iri name 3. att name 4. att iri name
        elif intent == 'rotational_relaxation_collision':
            q = RELAXATION_COLLISION % species
            rst = self.fire_query_to_ldf_ontokin(q, None, None).decode('utf-8')

        else:
            return None

        if rst is None:
            return None
        else:
            rst = json.loads(rst)
            rst = json.dumps(rst)
            return rst

    # Query ontocompchem ontology, the intent should be "query_quantum_chemistry"
    # However, due to the inconsistency in ontocompchem, we need to further classify the questions
    # currently, we use the attribute to find the according sub-intent and map question to templates.
    def query_quantum_of_moleculars(self, intent, species, attribute):
        if intent == 'electronic_energy':
            species = self.validator.validate(attribute, 'ontocompchem', intent, species).replace(' ', '')
            q = ELECTRONIC_ENERGY % species
            rst = self.fire_query_to_ldf_ontocompchem(q).decode('utf-8')
            return rst

        attribute_iri = self.attribute_mapper.find_closest_attribute(intent, attribute)
        species = self.validator.validate(attribute, 'ontocompchem', intent, species).replace(' ', '')
        _intent = self.attribute_mapper.map_to_quantum_queries(attribute_iri)

        if species is None:
            return None

        if _intent in intent_to_template_mapping:
            q = intent_to_template_mapping[_intent] % species
            rst = self.fire_query_to_ldf_ontocompchem(q).decode('utf-8')
            return rst

        else:
            return None

    # to find the reactions that meet the conditions first, then find the mechanism
    def query_mechanism_by_reaction(self, reactants, products):
        q = self.template_dict['select_mechanism_by_reaction']
        rst = self.fire_query_to_ldf_ontokin(q, products, reactants).decode('utf-8')
        return rst

    # to find reactions by reactants and products
    def query_by_reaction_only(self, reactants, products):
        query = self.template_dict['select_reaction_by_species']
        rst = self.fire_query_to_ldf_ontokin(query, products, reactants).decode('utf-8')
        return rst

    # if to query properties of reactions including reaction rate and whether the reaction is reversible
    def query_reaction_property(self, reactants, products, attribute):
        attribute = ' <' + self.serach_interface.get_first_match(attribute).strip() + '> '
        if 'hasArrheniusCoefficient' in attribute:
            query = self.template_dict['query_reaction_property']['ArrheniusCoefficient']
            rst = self.fire_query_to_ldf_ontokin(query, products, reactants).decode('utf-8')
            return rst

        elif 'isReversible' in attribute:
            query = self.template_dict['query_reaction_property']['isReversible']
            rst = self.fire_query_to_ldf_ontokin(query, products, reactants).decode('utf-8')
            return rst
