from pprint import pprint
import hashlib
import time
from owlready2 import get_ontology, DataProperty, ObjectProperty


class OntoAgentGenerator:

    def __init__(self, agent_name):
        # get the tbox of ontoagent
        ontoagent_tbox = 'http://www.theworldavatar.com/ontology/ontoagent/MSM.owl'
        onto = get_ontology(ontoagent_tbox).load()
        self.ontoagent = onto
        self.name = agent_name
        self.this_agent = get_ontology('http://www.theworldavatar.com/kb/agents/Service__%s.owl#' % self.name)
        self.qualifiers_dict = {}  # make a name -> object dict for qualifiers

        with self.ontoagent:
            class hasNerLabel(DataProperty):
                domain = [self.ontoagent.MessagePart]
                range = [str]

            class hasQuestionTemplates(DataProperty):
                domain = [self.ontoagent.Operation]
                range = [str]

            class hasQualifier(ObjectProperty):
                domain = [self.ontoagent.MessagePart]
                range = [self.ontoagent.MessagePart]

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
        pprint(self.ontoagent.hasNerLabel)
        pprint(self.ontoagent.hasQuestionTemplates)
        pprint(self.ontoagent.hasQualifier)

        self.ontoagent.save('OntoAgent.owl', format='rdfxml')

    # create the node id with hash
    def generate_id(self, mode, extra_info=''):
        content = (self.name + mode + extra_info).encode('utf-8')
        hash_object = hashlib.sha1(content)
        hex_dig = hash_object.hexdigest()
        return mode + '_' + str(hex_dig)

    def create_a_message_part(self, parameters):
        # data_name, data_type, is_array,
        data_name = parameters['data_name']
        data_type = parameters['data_type']
        is_array = parameters['is_array']
        ner_label = parameters['ner_label']
        message_part = self.ontoagent.MessagePart(self.generate_id('MessagePart',
                                                                   extra_info=data_name), namespace=self.this_agent)
        # 7. the most important part, declare its type, name, whether it is an array
        message_part.hasType.append(data_type)
        message_part.hasName.append(data_name)
        message_part.isArray.append(is_array)
        message_part.hasNerLabel.append(ner_label)
        return message_part

    def attach_input_output(self, operation, parameters, mode):
        data_name = parameters['data_name']
        # 4. create MessageContent
        message_content = self.ontoagent.MessageContent(self.generate_id('MessageContent', extra_info=data_name),
                                                        namespace=self.this_agent)
        # 5. attach the MessageContent to the operation
        if mode == 'input':
            operation.hasInput.append(message_content)
        else:
            operation.hasOutput.append(message_content)

        message_part = self.create_a_message_part(parameters)
        # 8. connect MessagePart to MessageContent
        message_content.hasMandatoryPart.append(message_part)

        if 'has_qualifier' in parameters:
            qualifier_name_list = parameters['has_qualifier']
            qualifiers_list = []
            for qualifier_name in qualifier_name_list:
                if qualifier_name in self.qualifiers_dict:
                    qualifier_object = self.qualifiers_dict[qualifier_name]
                    qualifier_message_part = self.create_a_message_part(qualifier_object)
                    qualifiers_list.append(qualifier_message_part)

            message_part.hasQualifier = qualifiers_list

    def get_qualifiers(self, agent_object):
        if 'qualifiers' in agent_object:
            qualifiers = agent_object['qualifiers']
            for qualifier in qualifiers:
                data_name = qualifier['data_name']
                self.qualifiers_dict[data_name] = qualifier

    def create_instance(self, agent_object):
        # 1. create service
        service = self.ontoagent.Service(self.generate_id('Service'), namespace=self.this_agent)

        # 2. create operation, attach the operation to the service
        operation = self.ontoagent.Operation(self.generate_id('Operation'), namespace=self.this_agent)
        service.hasOperation.append(operation)

        # 10. create the nodes for qualifiers, as message parts
        self.get_qualifiers(agent_object)
        pprint(self.qualifiers_dict)

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

        question_templates = agent_object['question_templates']
        # 11. add questions templates to operation node
        operation.hasQuestionTemplates = question_templates



if __name__ == '__main__':
    agent = {
        "question_templates": ['[%s](attribute) [%s](species)', '[%s](attribute) of [%s](species)'],
        "http_url": "http://somewhereincmcl.com/pce",
        "outputs": [
            {
                "data_name": "power conversion efficiency",
                "data_type": "http://fake_concept_for_power_conversion_efficiency",
                "is_array": False,
                "ner_label": "attribute"
            }
        ],
        "inputs": [
            {
                "data_name": "species",
                "data_type": "http://fake_concept_for_species",
                "is_array": False,
                "ner_label": "species"
            }
        ]
    }

    og = OntoAgentGenerator('PCE_Agent')
    og.create_instance(agent)
    og.this_agent.save('test', format='rdfxml')
