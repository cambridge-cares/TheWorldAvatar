# collect the labels of the inputs and outputs of the agent
# find their slot in the question template


# e.g. what is the [power conversion efficiency](output) of [smile string](input)
import json
import logging
import os
import random
from AgentPropertyQuery import AgentPropertyQuery
from location import FILE_DIR

with open(os.path.join(FILE_DIR, 'URI_SMILES_DICT')) as f:
    URI_SMILES_DICT = json.loads(f.read())


def get_instance_labels(ID):
    tmp = []
    with open('C:/data/instance_info/%s' % ID) as f:
        data = json.loads(f.read())['results']['bindings']
        for b in data:
            if 'label' in b:
                label = b['label']['value'].replace('[', '').strip()
            tmp.append(label.strip())
            if 'alt_label' in b:
                alt_label = [al.strip().replace('[', '') for al in b['altLabel_list']['value'].split('$')]
                tmp = tmp + alt_label
            if 'formula' in b:
                formula = b['formula']['value']
                subscript = str.maketrans("₀₁₂₃₄₅₆₇₈₉", "0123456789")
                formula = formula.translate(subscript)
                tmp.append(formula.strip().replace('[', ''))
    ID = 'http://www.wikidata.org/entity/' + ID
    if ID in URI_SMILES_DICT:
        SMILES = URI_SMILES_DICT[ID]
        tmp.append(SMILES)
    return set([t.replace('(', '').replace(')', '') for t in tmp])


def get_instances_by_data_type(data_type):
    labels = []
    species_id_list = get_species(500)
    for id in species_id_list:
        labels_of_species = get_instance_labels(id)
        labels = labels + list(labels_of_species)
    return labels


def create_labelled_questions(inputs_for_creating_questions):
    questions = []
    # input_for_creating_question = {'output_labels': data_nlp_label, 'output_ner_labels': data_ner_label,
    #                                'qualifier_names': qualifier_names, 'qualifier_ner_labels': qualifier_ner_labels,
    #                                'question_templates': question_templates, 'input_ner_labels': input_ner_labels,
    #                                'input_data_types': input_types}
    # 1. Go through the outputs first
    #       2. Go through the templates
    output_labels = inputs_for_creating_questions['output_labels']
    output_ner_label = inputs_for_creating_questions['output_ner_labels']
    templates = inputs_for_creating_questions['question_templates']
    qualifier_names = inputs_for_creating_questions['qualifier_names']
    qualifier_ner_labels = inputs_for_creating_questions['qualifier_ner_labels']

    tmp_templates = []

    for output_label in output_labels:
        for template in templates:
            # replace the placeholder with the nlp label of the output
            placeholder = '<%s>' % output_ner_label
            if placeholder in template:
                value = '[%s](%s)' % (output_label, output_ner_label)
                template = template.replace(placeholder, value)
                tmp_templates.append(template)
    templates = tmp_templates
    input_data_types = inputs_for_creating_questions['input_data_types']
    input_ner_labels = inputs_for_creating_questions['input_ner_labels']

    input_instance_dict = {}
    for input_data_type, input_ner_label in zip(input_data_types, input_ner_labels):
        instances = get_instances_by_data_type(input_data_type)
        input_instance_dict[input_ner_label] = instances
        input_placeholder = '<%s>' % input_ner_label
        for input_instance in instances:
            for template in templates:
                question = template
                if input_placeholder in template:
                    input_value = '[%s](%s)' % (input_instance, input_ner_label)
                    if input_instance.startswith('[') or input_instance.endswith(']'):
                        input_value = '[[%s]](%s)' % (input_instance, input_ner_label)

                    question = question.replace(input_placeholder, input_value, 1)
                    if len(qualifier_ner_labels) == 0:
                        if '<qualifier>' not in question:
                            questions.append(' - ' + question + '\n')

                for qualifier_name, qualifier_ner_label in zip(qualifier_names, qualifier_ner_labels):
                    qualifier_placeholder = '<%s>' % 'qualifier'
                    # 1. fill in the inputs value (species instances)
                    if qualifier_placeholder in question:
                        qualifier_content = generate_numerical_for_qualifiers(qualifier_name)
                        qualifier_value_1 = '%s [%s](%s)' % (qualifier_ner_label, qualifier_content, qualifier_ner_label)
                        qualifier_value_2 = '[%s](%s)' % (qualifier_content, qualifier_ner_label)
                        qualifier_value = random.choice([qualifier_value_1, qualifier_value_2])
                        question = question.replace(qualifier_placeholder, qualifier_value, 1)

                        if '<qualifier>' not in question:
                            questions.append(' - ' + question + '\n')

    return questions


def create_questions_from_agent(agent_name):
    questions = []
    original_questions = [] # store the questions in their original forms
    apq = AgentPropertyQuery()
    agent_attributes = apq.get_agent_attributes(agent_name)
    # 1. iterate through all the outputs
    agent_outputs = agent_attributes['outputs']
    agent_inputs = agent_attributes['inputs']
    question_templates = agent_attributes['templates']

    input_ner_labels = []
    input_types = []

    output_ner_labels_list = []
    output_nlp_labels_list = []

    for agent_input in agent_inputs:
        data_ner_label = agent_input['ner_label']
        data_type = agent_input['data_type']
        input_ner_labels.append(data_ner_label)
        input_types.append(data_type)

    for output in agent_outputs:
        data_ner_label = output['ner_label']
        data_nlp_label = output['data_nlp_label']
        qualifier_name = output['qualifier_name']

        output_ner_labels_list.append(data_ner_label)
        output_nlp_labels_list.append(data_nlp_label)

        # query the qualifiers
        if ',' in qualifier_name:
            qualifier_name_list = qualifier_name.split(',')
        else:
            qualifier_name_list = [qualifier_name]
        qualifier_names = []
        qualifier_ner_labels = []
        for qualifier_uri in qualifier_name_list:
            qualifier_dict = apq.get_agent_qualifiers(agent_name, qualifier_uri)
            if qualifier_dict is None:
                pass
            else:
                name = qualifier_dict['name']
                ner_label = qualifier_dict['ner_label']
                qualifier_names.append(name)
                qualifier_ner_labels.append(ner_label)

        input_for_creating_question = {'output_labels': data_nlp_label, 'output_ner_labels': data_ner_label,
                                       'qualifier_names': qualifier_names, 'qualifier_ner_labels': qualifier_ner_labels,
                                       'question_templates': question_templates, 'input_ner_labels': input_ner_labels,
                                       'input_data_types': input_types}

        questions = questions + create_labelled_questions(input_for_creating_question)
    questions_blk = ''.join(questions)
    block = '''## intent: %s\n %s ''' % (agent_name.replace('.owl', ''), questions_blk)
    original_questions_list = questions
    return block, questions, original_questions_list


def generate_numerical_for_qualifiers(qualifier):
    qualifier_unit_dict = {'temperature': ['kelvin', 'celsius',
                                           'fahrenheit', 'K', 'C', 'F', 'degree', 'degrees','degree celsius', 'degree fahrenheit', 'c', 'f', 'k'],
                           'pressure': ['bar', 'Pa', 'Pascal', 'atm', 'psi']}
    # upper limit, lower limit, steps
    number = random.randint(-500, 5000)
    unit = random.choice(qualifier_unit_dict[qualifier])
    q1 = '%s %s' % (number, unit)
    q2 = '%s%s' % (number, unit)
    q3 = '%s' % random.choice([number])
    q4 = '%s' % random.choice(['room temperature', 'room pressure'])
    q = random.choice([q1, q2, q3, q4])
    return q


def sample_instances(_type):
    # in the pce case, sample random species, find their smiles string, name, alter name
    pass


# TODO: OntoChemExp (347)
def get_species(number=500):
    with open(os.path.join(FILE_DIR, 'instance_property_mapping_first_2000')) as f:
        mapping = json.loads(f.read())
        instances = mapping.keys()
        return random.sample(instances, number)


def get_smiles(id_list):
    base_url = 'http://www.wikidata.org/entity/'
    rst = []
    with open(os.path.join(FILE_DIR, 'URI_SMILES_DICT')) as f:
        URI_SMILES_DICT = json.loads(f.read())
        for id in id_list:
            new_id = base_url + id
            if new_id in URI_SMILES_DICT:
                SMILE = URI_SMILES_DICT[new_id]
                SMILE = SMILE.replace('[', 'lb').replace(']', 'rb')
                rst.append(SMILE)
    print(rst)
    return rst


# def create_question():
#     apq = AgentPropertyQuery()
#     template_attribute_species = '- [%s](attribute) [%s](species)\n'
#     template_dict = {'attribute species': template_attribute_species}
#     agent_attributes = apq.get_agent_attributes('PCE_Agent.owl')
#     pprint(agent_attributes)
#     # get attributes of the agents
#     agent_id = agent_attributes['agent_id']
#     ner_labels = agent_attributes['ner_labels']
#     template = template_dict[ner_labels]
#
#     block = '## intent: %s \n ' % agent_id + '%s \n \n ' + '''
# ## intent: weather_agent
#    - [temperature](attribute) in [cambridge](city)
#     '''
#     SMILES = get_smiles(get_species())
#     attributes = ['power conversion efficiency', 'pce']
#     questions_text = ''
#     for SMILE in SMILES:
#         for attribute in attributes:
#             q = template % (attribute, SMILE)
#             questions_text = questions_text + q
#     block = block % questions_text
#     return block

logger = logging.getLogger('Function I/O')
logger.setLevel(logging.INFO)
blk_1, blk_1_questions, original_questions_1 = create_questions_from_agent('Thermo_Agent.owl')
logger.info('{} Thermo Agent Questions Are Created'.format(len(blk_1_questions)))
blk_2, blk_2_questions, original_questions_2 = create_questions_from_agent('PCE_Agent.owl')
logger.info('{} PCE    Agent Questions Are Created'.format(len(blk_2_questions)))

with open('./training/data/nlu.md', 'wb') as f:
    f.write(blk_1.encode('utf-8'))
    f.write(blk_2.encode('utf-8'))
    f.close()

# original_question = original_questions_1 + original_questions_2
selected_questions_for_evaluation_stdc = random.sample(original_questions_1, 50)
selected_questions_for_evaluation_pce = random.sample(original_questions_2, 50)

with open('./training/data/question_list_stdc', 'wb') as f:
    f.write(json.dumps(selected_questions_for_evaluation_stdc).encode('utf-8'))
    f.close()

with open('./training/data/question_list_pce', 'wb') as f:
    f.write(json.dumps(selected_questions_for_evaluation_pce).encode('utf-8'))
    f.close()



