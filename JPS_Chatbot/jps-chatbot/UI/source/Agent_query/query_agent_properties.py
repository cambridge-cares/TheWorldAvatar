from pprint import pprint

import rdflib


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
        SELECT DISTINCT ?type ?name ?isArray ?nerLabel
           WHERE {
              ?operation msm:hasOutput ?MessageContent . 
              ?MessageContent msm:hasMandatoryPart ?MessagePart . 
              ?MessagePart msm:hasType ?type ;
                           msm:hasName ?name ; 
                           msm:isArray ?isArray ;
                           msm:hasNerLabel ?nerLabel .
           }  
        """
    output_rst = g.query(get_output_query)

    outputs = []
    for row in output_rst:
        data_type = row['type'].value
        data_name = row['name'].value
        is_array = row['isArray'].value
        ner_label = row['nerLabel'].value
        ner_labels.append(ner_label)
        o = {'data_type': data_type, 'data_name': data_name, 'is_array': is_array, 'ner_label': ner_label}
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

    rst = {'parameters': inputs + outputs, 'agent_id': service_id, 'ner_labels': ' '.join(sorted(ner_labels))}
    return rst
