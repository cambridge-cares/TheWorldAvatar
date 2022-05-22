import json
import os
import urllib.parse
import urllib.request
from pprint import pprint

import rdflib

if __name__ == "__main__":
    from util.location import AGENT_OWL_DIR, FILE_DIR
else:
    from .util.location import AGENT_OWL_DIR, FILE_DIR


class AgentRequestConstructor:
    def __init__(self):
        self.base_url = "http://cmclinnovations.com"
        self.port = ""

    def call_agent(self, nlu_result):
        rst = self.proces_nlu_result(nlu_result)
        response = self.make_http(rst)
        return response

    def make_http(self, input_map):

        # input_map = {'input_map': {'smiles': ['CC']},
        #              'url': 'http://kg.cmclinnovations.com:5001/api/model/predict'}

        url = input_map['url']
        url += "?"
        values = {}
        parameters = []
        for key in input_map['input_map']:
            value = input_map['input_map'][key]
            if type(value) == type([]):
                value = key + '=[' + ','.join(value) + ']'
            else:
                value = key + '=' + value

            parameters.append(value)

        parameter_string = '&'.join(parameters)
        full_url = url + parameter_string
        print(full_url)
        req = urllib.request.Request(full_url)
        rst = urllib.request.urlopen(req).read()
        print(rst)
        # http://kg.cmclinnovations.com:5001/api/model/predict?smiles=[CC]
        # expect to receive {"smiles": ["CC"]}
        # request = 'http://kg.cmclinnovations.com:5001/api/model/predict?'

    def proces_nlu_result(self, nlu_result):
        # get the first intent, find the according agent
        agent_name = nlu_result['intent']['name']
        # query the agent by name
        request_info = self.get_agent_request_attributes(agent_name)
        # pprint(request_info)
        entities_from_nlu = {}
        for e in nlu_result['entities']:
            entity = e['entity']
            value = e['value']
            if entity not in entities_from_nlu:
                entities_from_nlu[entity] = [value]
            else:
                entities_from_nlu[entity].append(value)
        url = request_info['url']
        input_map = {}
        for input in request_info['inputs']:
            isArray = bool(input['is_array'])
            print('isArray', type(isArray))
            nerLabel = input['ner_label']
            dataName = input['data_name']
            # find the input from the nlu result, the name should match nerLabel
            if nerLabel in entities_from_nlu:
                input_value = entities_from_nlu[nerLabel]
                # put it into the agent parameter name
                # convert it to an array if isArray is true
                if isArray:
                    if type(input_value) == type([]):
                        input_map[dataName] = [i.upper() for i in input_value]
                    else:
                        input_map[dataName] = [input_value.upper()]
                else:
                    input_map[dataName] = input_value.upper()

        # query all the inputs required for the agent
        # find the has_name, has_ner_label property of the inputs, map the values back to the inputs names
        # find the isArray property
        # construct the input map for making the request
        return {'input_map': input_map, 'url': url}

    # # this function is
    # def hot_patch_for_thermo_agent(self, species):
    #     #

    def get_agent_request_attributes(self, agent_name):
        g = rdflib.Graph()
        agent_dir = os.path.join(FILE_DIR, agent_name)
        g.parse(agent_dir)
        agent_url = ''
        get_input_query = """
            PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
            SELECT DISTINCT ?type ?name ?isArray ?nerLabel
               WHERE {
                  ?operation msm:hasInput ?MessageContent . 
                  ?MessageContent msm:hasMandatoryPart ?MessagePart . 
                  ?MessagePart msm:hasType ?type ;
                               msm:hasName ?name ; 
                               msm:isArray ?isArray ;
                               msm:hasNerLabel ?nerLabel .
               }  
            """
        input_rst = g.query(get_input_query)
        inputs = []
        for row in input_rst:
            data_type = row['type'].value
            data_name = row['name'].value
            is_array = row['isArray'].value
            ner_label = row['nerLabel'].value
            i = {'data_type': data_type, 'data_name': data_name, 'is_array': is_array, 'ner_label': ner_label}
            inputs.append(i)

        get_url = """
            PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
            SELECT DISTINCT ?service ?http 
               WHERE {
                  ?service msm:hasOperation ?operation . 
                  ?operation msm:hasHttpUrl ?http .
               }  

        """
        url_rst = g.query(get_url)
        for row in url_rst:
            service_id = row['service']
            if '#' in service_id:
                service_id = service_id.split('#')[0].replace('http://www.theworldavatar.com/kb/agents/', '')
            http = row['http'].value
            agent_url = http

        return {'inputs': inputs, 'url': agent_url}
