import json, os
from pprint import pprint
from location import FILE_DIR
import rdflib


class AgentPropertyQuery:

    def __init__(self):
        self.label_mapping = {'http://fake_concept_for_general_thermal_property': ['thermal data', 'thermal property',
                                                                                   'thermal kinetic'],
                              'http://fake_concept_for_GibbsEnergy': ['gibbs energy'],
                              'http://fake_concept_for_entropy': ['entropy'],
                              'http://fake_concept_for_enthalpy': ['enthalpy'],
                              'http://fake_concept_for_InternalEnergy': ['internal energy'],
                              'http://fake_concept_for_heat_capacity': ['heat capacity'],
                              'http://fake_concept_for_heat_capacity_at_constant_pressure': [
                                  'heat capacity at constant pressure'],
                              'http://fake_concept_for_heat_capacity_at_constant_volume': [
                                  'heat capacity at constant volume'],
                              'http://fake_concept_for_power_conversion_efficiency': ['pce',
                                                                                      'power conversion efficiency',
                                                                                      'pce of OPF with donor',
                                                                                      'pce with donor',
                                                                                      'power conversion efficiency of OPF with donor']}

    def test_query(self, query, agent_name):
        '''
                    PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
            SELECT DISTINCT ?type ?name ?isArray ?nerLabel
               WHERE {
                  ?operation msm:hasOutput ?MessageContent .
                  ?MessageContent msm:hasMandatoryPart ?MessagePart .
                  ?MessagePart msm:hasType ?type ;
                               msm:hasName ?name ;
                               msm:isArray ?isArray ;
                               msm:hasNerLabel ?nerLabel .

               } GROUP BY ?type ?name ?isArray ?nerLabel
        '''

        g = rdflib.Graph()
        g.parse(os.path.join(FILE_DIR, agent_name))
        rst = g.query(query)

    def get_agent_qualifiers(self, agent_name, node_uri):
        g = rdflib.Graph()
        # ... add some triples to g somehow ...
        g.parse(os.path.join(FILE_DIR, agent_name))
        get_qualifier_query = '''
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        SELECT DISTINCT ?nerLabel ?name
        WHERE {
            <%s>       msm:hasNerLabel ?nerLabel ;
                       msm:hasName     ?name .
        }
        '''
        qualifier_rst = g.query(get_qualifier_query % node_uri)
        if len(qualifier_rst) == 0:
            return None
        for row in qualifier_rst:
            name = row['name'].value
            ner_label = row['nerLabel'].value

        return {'name': name, 'ner_label': ner_label}

    def get_agent_attributes(self, agent_name='PCE_Agent.owl'):
        ner_labels = []
        templates = []
        http_url = ''
        g = rdflib.Graph()
        # ... add some triples to g somehow ...
        g.parse(os.path.join(FILE_DIR, agent_name))
        get_essential_properties = """
            PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
            SELECT DISTINCT ?service ?http ?templates
               WHERE {
                  ?service msm:hasOperation ?operation . 
                  ?operation msm:hasHttpUrl ?http ;
                             msm:hasQuestionTemplates ?templates . 
                            
               }  
        
        """
        essential_rst = g.query(get_essential_properties)

        for row in essential_rst:
            service_id = row['service']
            if '#' in service_id:
                service_id = service_id.split('#')[0].replace('http://www.theworldavatar.com/kb/agents/', '')
            http_url = row['http']
            template = row['templates'].value
            templates.append(template)
        templates = list(set(templates))

        get_output_query_with_out_qualifier = '''
            PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
            SELECT DISTINCT ?type ?name ?isArray ?nerLabel 
               WHERE {
                  ?operation msm:hasOutput ?MessageContent . 
                  ?MessageContent msm:hasMandatoryPart ?MessagePart . 
                  ?MessagePart msm:hasType ?type ;
                               msm:hasName ?name ; 
                               msm:isArray ?isArray ;
                               msm:hasNerLabel ?nerLabel .
    
               } GROUP BY ?type ?name ?isArray ?nerLabel 
        '''

        get_output_query = """
            PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
            SELECT DISTINCT ?type ?name ?isArray ?nerLabel 
            (GROUP_CONCAT(?qualifier_name_candidates; separator=",") as ?qualifier_name)
               WHERE {
                  ?operation msm:hasOutput ?MessageContent . 
                  ?MessageContent msm:hasMandatoryPart ?MessagePart . 
                  ?MessagePart msm:hasType ?type ;
                               msm:hasName ?name ; 
                               msm:isArray ?isArray ;
                               msm:hasNerLabel ?nerLabel ;
                               msm:hasQualifier ?qualifier_name_candidates . 
              
               } GROUP BY ?type ?name ?isArray ?nerLabel ?qualifier_name
            """

        qualifier_test_query = '''
            PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
            SELECT DISTINCT ?qualifier
               WHERE {
                  ?operation msm:hasOutput ?MessageContent . 
                  ?MessageContent msm:hasMandatoryPart ?MessagePart . 
                  ?MessagePart msm:hasQualifier ?qualifier . 
               }  
        '''
        qualifier_test = g.query(qualifier_test_query)
        if len(qualifier_test) == 0:
            output_rst = g.query(get_output_query_with_out_qualifier)
        else:
            output_rst = g.query(get_output_query)

        outputs = []
        for row in output_rst:
            data_type = row['type'].value
            if data_type in self.label_mapping:
                data_nlp_label = self.label_mapping[data_type]
                ner_label = row['nerLabel'].value
                ner_labels.append(ner_label)

            else:
                data_nlp_label = []
            data_name = row['name'].value
            is_array = row['isArray'].value
            try:
                qualifier_name = str(row['qualifier_name'])
            except(KeyError):
                qualifier_name = []


            o = {'data_type': data_type, 'data_name': data_name, 'is_array': is_array, 'ner_label': ner_label,
                 'qualifier_name': qualifier_name, 'data_nlp_label': data_nlp_label}
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
            ner_label = []
            data_type = row['type'].value
            data_name = row['name'].value
            is_array = row['isArray'].value
            ner_label = row['nerLabel'].value
            ner_labels.append(ner_label)
            i = {'data_type': data_type, 'data_name': data_name, 'is_array': is_array, 'ner_label': ner_label}
            inputs.append(i)
        # rst = {'inputs': inputs, 'outputs': outputs, 'agent_id': service_id, 'ner_labels': ' '.join(sorted(
        # ner_labels))}
        rst = {'inputs': inputs, 'outputs': outputs, 'agent_id': service_id, 'http_url': http_url,
               'templates': templates}
        return rst



if __name__ == '__main__':
    apq = AgentPropertyQuery()
    rst = apq.get_agent_attributes('PCE_Agent.owl')
    print(json.dumps(rst, indent=4))
    print('=======================================\n\n\n\n\n')
    rst = apq.get_agent_attributes('Thermo_Agent.owl')
    print(json.dumps(rst, indent=4))
    # test_q = '''
    #             PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
    #         SELECT DISTINCT ?type ?name ?isArray ?nerLabel
    #            WHERE {
    #               ?operation msm:hasOutput ?MessageContent .
    #               ?MessageContent msm:hasMandatoryPart ?MessagePart .
    #               ?MessagePart msm:hasType ?type ;
    #                            msm:hasName ?name ;
    #                            msm:isArray ?isArray ;
    #                            msm:hasNerLabel ?nerLabel .
    #
    #            } GROUP BY ?type ?name ?isArray ?nerLabel
    # '''
    # apq.test_query(test_q, 'PCE_Agent.owl')

    # rst = apq.get_agent_attributes('Thermo_Agent.owl')
    # print(json.dumps(rst, indent=4))
