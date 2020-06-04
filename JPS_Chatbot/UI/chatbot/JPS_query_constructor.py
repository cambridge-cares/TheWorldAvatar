import json
import urllib.parse
import urllib.request
from pprint import pprint

from .locations import JPS_SPARQL_TEMPLATE_PATH
from .search_interface import SearchInterface


class JPS_query_constructor:

    def __init__(self):
        with open(JPS_SPARQL_TEMPLATE_PATH) as f:
            self.template_dict = json.loads(f.read())
        self.serach_interface = SearchInterface()

    @staticmethod
    def extract_info(intents):
        intent = intents['intent']['name']
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
        print('=================== intents ================')
        pprint(intents)
        result = JPS_query_constructor.extract_info(intents)
        intent = result['intent']
        if intent == 'query_reaction_property':
            try:
                rst = self.query_reaction_property(result['reactants'], result['products'], result['attribute'])
            except:
                rst = self.query_reaction_property(result['reactants'], result['products'], result['indicator'])
        elif intent == 'select_reaction_by_species':
            # TODO: seperate reactants and products
            rst = self.query_by_reaction_only(result['reactants'], result['products'])
        elif intent == 'select_mechanism_by_reaction':
            rst = self.query_mechanism_by_reaction(result['reactants'], result['products'])
        return rst

    def query_mechanism_by_reaction(self, reactants, products):
        q = self.construct_query_reaction_by_species(reactants, products)
        q = q % ('?MechanismName', '''\n ?reaction ontokin:belongsToPhase ?Phase .
?Phase ontokin:containedIn ?MechanismIRI .
?MechanismIRI rdfs:label ?MechanismName .''')
        rst = self.fire_query(q).decode('utf-8')

        return rst

    def query_by_reaction_only(self, reactants, products):
        q = self.construct_query_reaction_by_species(reactants, products) % ('', '')
        print(q)
        rst = self.fire_query(q).decode('utf-8')
        print(rst)
        return rst

        # TODO: construct the query by only reactants and products

    def query_reaction_property(self, reactants, products, attribute):
        sub_properties = []
        sub_properties_arrhenius = ['ontokin:hasActivationEnergy', 'ontokin:hasActivationEnergyUnits ',
                                    'ontokin:hasPreExponentialFactor', 'ontokin:hasPreExponentialFactorUnits',
                                    'ontokin:hasTemperatureExponent', 'ontokin:hasTemperatureExponentUnits']
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
            new_labels = ' ?%s '% propertyName


        sub_query = ''
        for num, p in enumerate(sub_properties, start=1):
            l = p.replace('ontokin:has', '')
            query_line = '?%s ' % propertyName + p + ' ?' + l + ' .\n '
            sub_query = sub_query + query_line
            new_labels = new_labels + ' ?' + l
        print('=============== new labels ==============')
        print(new_labels)
        print('-------------- sub_query -------------')
        print(sub_query)

        q = self.construct_query_reaction_by_species(reactants, products) % (
            new_labels, '\n{\n ?reaction %s ?%s . \n %s  }' % (attribute, propertyName, sub_query))
        print(q)
        rst = self.fire_query(q).decode('utf-8')
        return rst

    def fire_query(self, query):
        url = "http://www.theworldavatar.com/OntoKinGUI/OntoKinEndpointProxy"
        values = {'queryString': query}
        data = urllib.parse.urlencode(values).encode('utf-8')
        print(type(data))
        req = urllib.request.Request(url, data)
        response = urllib.request.urlopen(req).read()
        return response

    def construct_query_reaction_by_species(self, reactants, products):
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
