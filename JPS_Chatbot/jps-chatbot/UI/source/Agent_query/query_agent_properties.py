from pprint import pprint

import rdflib

label_mapping = {'http://fake_concept_for_GibbsEnergy': ['gibbs energy'],
                 'http://fake_concept_for_entropy': ['entropy'],
                 'http://fake_concept_for_enthalpy': ['enthalpy'],
                 'http://fake_concept_for_InternalEnergy': ['internal energy'],
                 'http://fake_concept_for_heat_capacity': ['heat capacity', 'heat capacity at constant pressure', 'HeatCapacityAtConstPressure']
                 }


def get_agent_qualifiers(agent_name, node_uri):

    g = rdflib.Graph()
    # ... add some triples to g somehow ...
    g.parse(agent_name)
    qualifier_nodes = []
    get_qualifier_query = '''
    PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
    SELECT DISTINCT ?nerLabel ?name
    WHERE {
        <%s>       msm:hasNerLabel ?nerLabel ;
                   msm:hasName     ?name .
    }
    '''
    qualifier_rst = g.query(get_qualifier_query%node_uri)
    for row in qualifier_rst:
        name = (row['name'].value)
        ner_label = (row['nerLabel'].value)
        print('name', name)
        print('ner label', ner_label)


def get_agent_attributes(agent_name='PCE_Agent.owl'):

    ner_labels = []
    g = rdflib.Graph()
    # ... add some triples to g somehow ...
    g.parse(agent_name)
    get_essential_properties = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        SELECT DISTINCT ?service ?http 
           WHERE {
              ?service msm:hasOperation ?operation . 
              ?operation msm:hasHttpUrl ?http .
           }  
    
    """
    essential_rst = g.query(get_essential_properties)
    print('\n====================\n')
    for row in essential_rst:
        service_id = row['service']
        if '#' in service_id:
            service_id = service_id.split('#')[0].replace('http://www.theworldavatar.com/kb/agents/', '')
        http = row['http']
        print(service_id)
        print(http)
    print('\n==================== \n')

    get_output_query = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        SELECT DISTINCT ?type ?name ?isArray ?nerLabel ?qualifier_name
           WHERE {
              ?operation msm:hasOutput ?MessageContent . 
              ?MessageContent msm:hasMandatoryPart ?MessagePart . 
              ?MessagePart msm:hasType ?type ;
                           msm:hasName ?name ; 
                           msm:isArray ?isArray ;
                           msm:hasNerLabel ?nerLabel ;
                           msm:hasQualifier ?qualifier_name . 
                           
                           
           }  
        """
    output_rst = g.query(get_output_query)

    outputs = []
    for row in output_rst:
        data_type = row['type'].value
        data_name = row['name'].value
        is_array = row['isArray'].value
        ner_label = row['nerLabel'].value
        qualifier_name = str(row['qualifier_name'])
        ner_labels.append(ner_label)
        o = {'data_type': data_type, 'data_name': data_name, 'is_array': is_array, 'ner_label': ner_label, 'qualifier_name': qualifier_name}
        outputs.append(o)

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
        ner_labels.append(ner_label)
        i = {'data_type': data_type, 'data_name': data_name, 'is_array': is_array, 'ner_label': ner_label}
        inputs.append(i)

    rst = {'inputs': inputs, 'outputs': outputs, 'agent_id': service_id, 'ner_labels': ' '.join(sorted(ner_labels))}
    return rst


rst = get_agent_attributes('Thermo_Agent.owl')
pprint(rst)
for output in rst['outputs']:
    print(output)

# get_agent_qualifiers('Thermo_Agent.owl')