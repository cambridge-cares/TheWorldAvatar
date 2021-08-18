# collect the labels of the inputs and outputs of the agent
# find their slot in the question template


# e.g. what is the [power conversion efficiency](output) of [smile string](input)
import json
from pprint import pprint
from random import sample
from query_agent_properties import get_agent_attributes


def sample_instances(_type):
    # in the pce case, sample random species, find their smiles string, name, alter name
    pass


# TODO: OntoChemExp (347)
def get_species(number=500):
    with open('../../data_preparation/Wiki_basic_info/instance_property_mapping_first_2000') as f:
        mapping = json.loads(f.read())
        instances = mapping.keys()
        return sample(instances, number)


def get_smiles(id_list):
    base_url = 'http://www.wikidata.org/entity/'
    rst = []
    with open('../../data_preparation/Wiki_basic_info/URI_SMILES_DICT') as f:
        URI_SMILES_DICT = json.loads(f.read())
        for id in id_list:
            SMILE = URI_SMILES_DICT[base_url + id]
            SMILE = SMILE.replace('[', 'lb').replace(']', 'rb')
            rst.append(SMILE)
    print(rst)
    return rst


def create_question():
    template_attribute_species = '- [%s](attribute) [%s](species)\n'
    template_dict = {'attribute species': template_attribute_species}
    agent_attributes = get_agent_attributes('PCE_Agent.owl')
    pprint(agent_attributes)
    # get attributes of the agents
    agent_id = agent_attributes['agent_id']
    parameters = agent_attributes['parameters']
    ner_labels = agent_attributes['ner_labels']
    template = template_dict[ner_labels]

    block = '## intent: %s \n ' % agent_id + '%s \n \n ' + '''
## intent: weather_agent
   - [temperature](attribute) in [cambridge](city)
    '''
    SMILES = get_smiles(get_species())
    attributes = ['power conversion efficiency', 'pce']
    questions_text = ''
    for SMILE in SMILES:
        for attribute in attributes:
            q = template % (attribute, SMILE)
            questions_text = questions_text + q
    block = block % questions_text
    return block


blk = create_question()
with open('./training/data/nlu.md', 'w') as f:
    f.write(blk)
    f.close()
