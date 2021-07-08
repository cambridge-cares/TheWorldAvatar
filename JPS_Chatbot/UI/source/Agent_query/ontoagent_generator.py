from pprint import pprint
import hashlib
import time
from owlready2 import *


class OntoAgentGenerator:

    def __init__(self, agent_name):
        # get the tbox of ontoagent
        ontoagent_tbox = 'http://www.theworldavatar.com/ontology/ontoagent/MSM.owl'
        onto = get_ontology(ontoagent_tbox).load()
        self.ontoagent = onto
        self.name = agent_name
        self.this_agent = get_ontology('http://www.theworldavatar.com/kb/agents/Service__%s.owl#' % self.name)

        # check the integrity of the core classes and attributes
        pprint(self.ontoagent.Service)
        pprint(self.ontoagent.Operation)  # http://www.theworldavatar.com/kb/agents/Service__DFT.owl#
        pprint(self.ontoagent.MessagePart)
        pprint(self.ontoagent.MessageContent)
        pprint(self.ontoagent.hasOutput)
        pprint(self.ontoagent.hasInput)
        pprint(self.ontoagent.hasHttpUrl)
        pprint(self.ontoagent.isArray)
        pprint(self.ontoagent.hasName)
        pprint(self.ontoagent.hasType)
        pprint(self.ontoagent.hasOperation)
        pprint(self.ontoagent.hasMandatoryPart)

    # create the node id with hash
    def generate_id(self, mode, extra_info=''):
        content = (self.name + mode + extra_info + str(time.ctime())).encode('utf-8')
        hash_object = hashlib.sha1(content)
        hex_dig = hash_object.hexdigest()
        return mode + '_' + str(hex_dig)

    def attach_input_output(self, operation, parameters, mode):

        # data_name, data_type, is_array,
        data_name = parameters['data_name']
        data_type = parameters['data_type']
        is_array = parameters['is_array']

        # 4. create MessageContent
        message_content = self.ontoagent.MessageContent(self.generate_id('MessageContent', extra_info=data_name),
                                                        namespace=self.this_agent)

        # 5. attach the MessageContent to the operation
        if mode == 'input':
            operation.hasInput.append(message_content)
        else:
            operation.hasOutput.append(message_content)

        # 6. create the MessagePart
        message_part = self.ontoagent.MessagePart(self.generate_id('MessagePart',
                                                                   extra_info=data_name), namespace=self.this_agent)

        # 7. the most important part, declare its type, name, whether it is an array
        message_part.hasType.append(data_type)
        message_part.hasName.append(data_name)
        message_part.isArray.append(is_array)
        # 8. connect MessagePart to MessageContent
        message_content.hasMandatoryPart.append(message_part)

    def create_instance(self, agent_object):
        # 1. create service
        service = self.ontoagent.Service(self.generate_id('Service'), namespace=self.this_agent)

        # 2. create operation, attach the operation to the service
        operation = self.ontoagent.Operation(self.generate_id('Operation'), namespace=self.this_agent)
        service.hasOperation.append(operation)

        # 3. give the operation a url
        http_url = agent_object['http_url']
        operation.hasHttpUrl.append(http_url)

        # 9. attach the input/output
        inputs = agent_object['inputs']
        for input in inputs:
            self.attach_input_output(operation, input, 'input')

        outputs = agent_object['outputs']
        for output in outputs:
            self.attach_input_output(operation, output, 'output')


if __name__ == '__main__':
    agent = {
        "http_url": "http://somewhereincmcl.com/pce",
        "outputs": [
            {
                "data_name": "power conversion efficiency",
                "data_type": "http://fake_concept_for_power_conversion_efficiency",
                "is_array": False
            }
        ],
        "inputs": [
            {
                "data_name": "species",
                "data_type": "http://fake_concept_for_species",
                "is_array": True
            }
        ]
    }

    og = OntoAgentGenerator('PCE_Agent')
    og.create_instance(agent)
    og.this_agent.save('test', format='rdfxml')
