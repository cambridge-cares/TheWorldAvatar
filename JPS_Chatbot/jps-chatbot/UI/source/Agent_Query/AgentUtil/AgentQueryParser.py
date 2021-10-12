import os
from pprint import pprint
import rdflib
from rapidfuzz import process, fuzz

if __name__ == '__main__':
    from util.SPARQLWarehouse import GET_AGENT_INPUT_PARAMETERS, GET_AGENT_OUTPUTS, GET_HTTP_URL
    from util.location import FILE_DIR
else:
    from .util.SPARQLWarehouse import GET_AGENT_INPUT_PARAMETERS, GET_AGENT_OUTPUTS, GET_HTTP_URL
    from .util.location import FILE_DIR


# extract inputs from the SPARQL query
def create_input_dict(_input_rst):
    # ?type ?name ?isArray ?nerLabel ?qualifiers)
    input_dict = {}
    for _o in _input_rst:
        name = _o['name'].value
        label = _o['nerLabel'].value
        input_dict[label] = name
    return input_dict


# find the qualifiers from the NLU results
def extract_qualifiers(_entities):
    qualifiers = []
    for _e in _entities:
        _e_label = _e['entity']
        if _e_label == 'qualifier':  # you are a qualifier, lets discuss your fate
            qualifiers.append(_e['value'])
    # TODO: get their types
    return qualifiers

    # get the labels of the qualifiers


# parse the outputs results returned from the SPARQL query
def create_output_dict(_output_rst):
    # ?type ?name ?isArray ?nerLabel ?qualifiers)
    output_dict = {}
    for _o in _output_rst:
        name = _o['name'].value
        label = _o['nerLabel'].value
        qualifiers = _o['qualifiers'].value
        if '; ' in qualifiers:
            qualifiers = qualifiers.split('; ')
        else:
            qualifiers = [qualifiers]
        output_dict[name] = {'label': label, 'qualifiers': qualifiers}
    return output_dict


# match between the inputs from the SPARQL query and the entities from the NLU result
# e.g. the input from the SPARQL is "species", then you need to find the entity in the NLU result, where the
# label of the entity is "species"
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


# match between the outputs from the SPARQL query and the NLU results
# e.g. in the NLU result, you might find {'enthalpy': 'attribute'}, while in
# SPARQL query result, you might find an output with a name "enthalpy" and label "attribute"
# However, the match needs to be fuzzy when comparing the names but explicit when comparing the labels
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
        output_label = _output_parameters_dict[output_name]['label']
        qualifier_labels = _output_parameters_dict[output_name]['qualifiers']
        valid_qualifier_labels = []
        # score larger than 80, attributes match
        if score > 80 and (_e_label == output_label):
            # here is a match, this is an output
            # find the qualifiers
            for _q_name in qualifier_labels:
                for _e_qualifier in _entities:
                    _e_qualifier_name = _e_qualifier['entity']
                    _e_qualifier_value = _e_qualifier['value']
                    if _q_name == _e_qualifier_name:
                        valid_qualifier_labels.append({'label': _e_qualifier_name, 'value': _e_qualifier_value})

            tmp = {'label': output_label, 'value': output_name, 'qualifiers': valid_qualifier_labels}
            outputs.append(tmp)

    return outputs


class AgentQueryParser:
    def __init__(self):
        self.base_url = "http://cmclinnovations.com"
        self.port = ""
        self.graph = rdflib.Graph()

    def parse(self, _nlu_result):
        # get the intent
        agent_name = _nlu_result['intent']['name']
        entities = _nlu_result['entities']
        output_rst, input_rst, url_rst = self.get_agent_request_attributes(agent_name)
        output_dict = create_output_dict(_output_rst=output_rst)
        _outputs = match_outputs(_entities=entities, _output_parameters_dict=output_dict)
        input_dict = create_input_dict(_input_rst=input_rst)
        _inputs = match_inputs(_entities=entities, _input_parameters_dict=input_dict)
        for _u in url_rst:
            _url = _u['url'].value
        return _inputs, _outputs, _url

    # query the OntoAgent instance via SPARQL and extract the inputs/outputs parameters of the agent
    def get_agent_request_attributes(self, agent_name):
        agent_dir = os.path.join(FILE_DIR, agent_name) + '.owl'
        print('agent dir', agent_dir)
        self.graph.parse(agent_dir)
        input_rst = self.graph.query(GET_AGENT_INPUT_PARAMETERS)
        # species fits species as the ner label
        output_rst = self.graph.query(GET_AGENT_OUTPUTS)
        # get the inputs and outputs, but also
        url_rst = self.graph.query(GET_HTTP_URL)
        return output_rst, input_rst, url_rst


if __name__ == '__main__':
    nlu_result = {'entities': [{'confidence_entity': 0.9874482978773302,
                                'end': 8,
                                'entity': 'attribute',
                                'extractor': 'CRFEntityExtractor',
                                'start': 0,
                                'value': 'entropy'},
                               {'confidence_entity': 0.8038620659230102,
                                'end': 42,
                                'entity': 'species',
                                'extractor': 'CRFEntityExtractor',
                                'start': 9,
                                'value': 'inchi=1s/c2h6o/c1-2-3/h3h,2h2,1h3'},
                               {'confidence_entity': 0.8924032698970731,
                                'end': 55,
                                'entity': 'pressure',
                                'extractor': 'CRFEntityExtractor',
                                'start': 46,
                                'value': '1522.1 pa'},
                               {'confidence_entity': 0.9948107351393644,
                                'end': 68,
                                'entity': 'temperature',
                                'extractor': 'CRFEntityExtractor',
                                'start': 60,
                                'value': '123245 k'}],
                  'intent': {'confidence': 1.0, 'name': 'Thermo_Agent'},
                  'intent_ranking': [{'confidence': 1.0, 'name': 'Thermo_Agent'},
                                     {'confidence': 5.383306859463607e-32, 'name': 'PCE_Agent'}],
                  'text': 'enthalpy inchi=1s/c2h6o/c1-2-3/h3h,2h2,1h3 at 1522.1 pa and 123245 k'}

    aqp = AgentQueryParser()
    inputs, outputs, url = aqp.parse(_nlu_result=nlu_result)
    print('inputs', inputs)
    print('outputs', outputs)
    print('url', url)