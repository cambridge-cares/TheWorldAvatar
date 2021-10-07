import os
from pprint import pprint

import rdflib
from rapidfuzz import process, fuzz
from SPARQLQueryWarehouse import GET_AGENT_INPUT_PARAMETERS, GET_AGENT_OUTPUTS
from location import FILE_DIR


def create_input_dict(_input_rst):
    # ?type ?name ?isArray ?nerLabel ?qualifiers)
    input_dict = {}
    for _o in _input_rst:
        name = _o['name'].value
        label = _o['nerLabel'].value
        input_dict[label] = name
    return input_dict


def extract_qualifiers( _entities):
    qualifiers = []
    for _e in _entities:
        _e_label = _e['entity']
        if _e_label == 'qualifier': # you are a qualifier, lets discuss your fate
            qualifiers.append(_e['value'])
    # TODO: get their types 
    return qualifiers

    # get the labels of the qualifiers


def create_output_dict(_output_rst):
    # ?type ?name ?isArray ?nerLabel ?qualifiers)
    output_dict = {}
    for _o in _output_rst:
        name = _o['name'].value
        label = _o['nerLabel'].value
        output_dict[name] = label
    return output_dict


def match_inputs(_entities, _input_parameters_dict):  # _output_parameters_dict {'species': 'species'}
    # inputs are simpler, just match the exact labels
    inputs = []  # you need the nerlabel: value
    candidate_names = _input_parameters_dict.keys()
    for _e in _entities:
        _e_value = _e['value']
        _e_label = _e['entity']
        # if you have one of the nerlabels, you input
        if _e_label in candidate_names:
            tmp = {'label': _e_label, 'value': _e_value}
            inputs.append(tmp)
    return inputs


def match_outputs(_entities, _output_parameters_dict):  # _output_parameters_dict {'enthalpy': 'attribute'}
    # find a matching output for each entity, if there is any
    # make a list of _output_parameters names for fuzzy match
    outputs = []
    candidate_names = _output_parameters_dict.keys()
    for _e in _entities:
        _e_name = _e['value']
        _e_label = _e['entity']
        rst = process.extractOne(_e_name, candidate_names, scorer=fuzz.ratio)
        output_name = rst[0]
        score = rst[1]
        output_label = _output_parameters_dict[output_name]
        # score larger than 70, attributes match
        if score > 70 and (_e_label == output_label):
            # here is a match, this is an output
            tmp = {'label': output_label, 'name': output_name}
            outputs.append(tmp)

    return outputs


class AgentQueryParser:
    def __init__(self):
        self.base_url = "http://cmclinnovations.com"
        self.port = ""
        self.graph = rdflib.Graph()

    def process_nlu_results(self, _nlu_result):
        # get the intent
        agent_name = _nlu_result['intent']['name']
        entities = _nlu_result['entities']
        io_results = self.get_agent_request_attributes(agent_name)

        output_rst = io_results[0]
        input_rst = io_results[1]

        output_dict = create_output_dict(_output_rst=output_rst)
        outputs = match_outputs(_entities=entities, _output_parameters_dict=output_dict)

        input_dict = create_input_dict(_input_rst=input_rst)
        inputs = match_inputs(_entities=entities, _input_parameters_dict=input_dict)

        qualifiers = extract_qualifiers(_entities=entities)
        print(qualifiers)

    def get_agent_request_attributes(self, agent_name):
        agent_dir = os.path.join(FILE_DIR, agent_name) + '.owl'
        self.graph.parse(agent_dir)
        input_rst = self.graph.query(GET_AGENT_INPUT_PARAMETERS)
        for _input in input_rst:
            print('Type: %s \nName: %s \nisArray: %s \nnerLabel: %s \n' % _input)
        # species fits species as the ner label
        # TODO: match identified stuff with the ner labels

        # TODO:
        print('===========================================================')
        output_rst = self.graph.query(GET_AGENT_OUTPUTS)
        return output_rst, input_rst


if __name__ == '__main__':
    nlu_result = {'entities': [{'confidence_entity': 0.9874482978773302,
                                'end': 8,
                                'entity': 'attribute',
                                'extractor': 'CRFEntityExtractor',
                                'start': 0,
                                'value': 'enthalpy'},
                               {'confidence_entity': 0.8038620659230102,
                                'end': 42,
                                'entity': 'species',
                                'extractor': 'CRFEntityExtractor',
                                'start': 9,
                                'value': 'inchi=1s/c2h6o/c1-2-3/h3h,2h2,1h3'},
                               {'confidence_entity': 0.8924032698970731,
                                'end': 55,
                                'entity': 'qualifier',
                                'extractor': 'CRFEntityExtractor',
                                'start': 46,
                                'value': '1522.1 pa'},
                               {'confidence_entity': 0.9948107351393644,
                                'end': 68,
                                'entity': 'qualifier',
                                'extractor': 'CRFEntityExtractor',
                                'start': 60,
                                'value': '123245 k'}],
                  'intent': {'confidence': 1.0, 'name': 'Thermo_Agent'},
                  'intent_ranking': [{'confidence': 1.0, 'name': 'Thermo_Agent'},
                                     {'confidence': 5.383306859463607e-32, 'name': 'PCE_Agent'}],
                  'text': 'enthalpy inchi=1s/c2h6o/c1-2-3/h3h,2h2,1h3 at 1522.1 pa and 123245 k'}
    aqp = AgentQueryParser()
    aqp.process_nlu_results(_nlu_result=nlu_result)
