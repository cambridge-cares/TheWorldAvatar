from SPARQLWrapper import SPARQLWrapper, JSON
import pandas as pd
import os
import sys
import json
from sklearn.model_selection import train_test_split
sys.path.append("")
from KGToolbox.Tools.CreateNegSamplingDictionary import NegSamplingCreator
from Marie.CandidateSelection.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor
from KGToolbox.Tools import MakeIndex


class OntoAgentReader:

    def __init__(self, agent, qualifier_exists, namespace):
        self.agent = agent
        self.full_dataset_dir = os.path.join(DATA_DIR, "../CrossGraph", self.agent)
        self.qualifier = qualifier_exists
        self.namespace=namespace

    def query_blazegraph(self, query):
        sparql = SPARQLWrapper("http://127.0.0.1:9999/blazegraph/namespace/" + self.namespace + "/sparql")
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return results

    def get_operation_output(self):
        triples = []

        query = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        select DISTINCT ?operation ?msg_part 
        where { 
                ?operation rdf:type <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>.
                ?operation msm:hasOutput ?msg_content.
                ?msg_content msm:hasMandatoryPart ?msg_part.
        }
        """

        result = self.query_blazegraph(query)["results"]["bindings"]
        
        for binding in result:
            operation = binding["operation"]["value"].split("#")[-1]
            message_part = binding["msg_part"]["value"].split("#")[-1]
            row = (operation, "hasOutput", message_part)
            triples.append(row)
        
        return triples

    def get_operation_input(self):
        triples = []

        query = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        select DISTINCT ?operation ?msg_part 
        where { 
                ?operation rdf:type <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>.
                ?operation msm:hasInput ?msg_content.
                ?msg_content msm:hasMandatoryPart ?msg_part.
        }
        """

        result = self.query_blazegraph(query)["results"]["bindings"]
        
        for binding in result:
            operation = binding["operation"]["value"].split("#")[-1]
            message_part = binding["msg_part"]["value"].split("#")[-1]
            row = (operation, "hasInput", message_part)
            triples.append(row)
        
        return triples

    def get_resource_properties(self):
        triples = []

        query = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        select DISTINCT ?msg_part ?class ?name ?label ?isArray
         where { 
            ?msg_part rdf:type <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#MessagePart>;
   	   		msm:hasType ?class;
    		msm:hasName ?name;
    		msm:hasNerLabel ?label;
    		msm:isArray ?isArray.
        }
        """

        result = self.query_blazegraph(query)["results"]["bindings"]
        
        for binding in result:
            message_part = binding["msg_part"]["value"].split("#")[-1]
            resource_class = binding["class"]["value"].split("#")[-1]
            name = binding["name"]["value"]
            ner_label = binding["label"]["value"]
            isArray = binding["isArray"]["value"]
            class_row = (message_part, "hasType", resource_class)
            name_row = (message_part, "hasName", name)
            ner_label_row = (message_part, "hasNerLabel", ner_label)
            isArray_row = (message_part, "isArray", isArray)
            
            # Append each tuple to the list
            triples.append(class_row)
            triples.append(name_row)
            triples.append(ner_label_row)
            triples.append(isArray_row)

        return triples
    
    def get_qualifier_for_output(self):
        triples = []

        query = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        select DISTINCT ?msg_part ?qualifier
        where { 
            ?msg_part rdf:type <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#MessagePart>;
   			msm:hasQualifier ?qualifier;
        }
        """

        result = self.query_blazegraph(query)["results"]["bindings"]
        
        for binding in result:
            message_part = binding["msg_part"]["value"].split("#")[-1]
            qualifier = binding["qualifier"]["value"].split("#")[-1]
            row = (message_part, "hasQualifier", qualifier)
            triples.append(row)
        
        return triples


    def get_operation_question_templates(self):
        triples = []

        query = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        select DISTINCT ?operation ?ques
        where { 
            ?operation rdf:type <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>.
            ?operation msm:hasQuestionTemplates ?ques
        }
        """

        result = self.query_blazegraph(query)["results"]["bindings"]
        
        for binding in result:
            operation = binding["operation"]["value"].split("#")[-1]
            ques_template = binding["ques"]["value"]
            row = (operation, "hasQuestionTemplates", ques_template)
            triples.append(row)
        
        return triples

    def get_http_url(self):
        triples = []

        query = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        select DISTINCT ?operation ?url 
        where { 
            ?operation rdf:type <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>.
            ?operation msm:hasHttpUrl ?url.
        }
        """

        result = self.query_blazegraph(query)["results"]["bindings"]
        
        for binding in result:
            operation = binding["operation"]["value"].split("#")[-1]
            url = binding["url"]["value"]
            row = (operation, "hasHttpUrl", url)
            triples.append(row)
        
        return triples
    
    def get_service(self):
        triples = []

        query = """
        PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
        select DISTINCT ?service ?operation
        where { 
            ?operation rdf:type <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>.
            ?service msm:hasOperation ?operation
        }
        """

        result = self.query_blazegraph(query)["results"]["bindings"]
        
        for binding in result:
            service = binding["service"]["value"].split("#")[-1]
            operation = binding["operation"]["value"].split("#")[-1]
            row = (service, "hasOperation", operation)
            triples.append(row)
        
        return triples    
    
    def write_triples_to_tsv(self, all_triples):
        df = pd.DataFrame(all_triples)
        df.to_csv(os.path.join(self.full_dataset_dir, f"{self.agent}-train.txt"),
                  sep="\t", header=False, index=False)

        df_train, df_test = train_test_split(df, test_size=0.1)
        df_train.to_csv(os.path.join(self.full_dataset_dir, f"{self.agent}-train-2.txt"),
                        sep="\t", header=False, index=False)

        df_test.to_csv(os.path.join(self.full_dataset_dir, f"{self.agent}-test.txt"),
                       sep="\t", header=False, index=False)
        
    def create_info_dictionaries(self):
        data = []
        with open(os.path.join(self.full_dataset_dir, f"{self.agent}-train.txt"), 'r') as file:
            for line in file:
                row = line.strip().split('\t')
                data.append(tuple(row))

        input_msg_part = set()
        output_msg_part = set()
        qualifier_msg_part = set()

        for row in data:
            if row[1]=='hasInput':
                input_msg_part.add(row[2])
            elif row[1] == 'hasOutput':
                output_msg_part.add(row[2])
            elif row[1] == 'hasQualifier':
                qualifier_msg_part.add(row[2])

        inputs = []
        outputs = []
        qualifiers = []

        for row in data:
            if row[0] in input_msg_part and row[1] == 'hasType':
                inputs.append(row[2])
            elif row[0] in output_msg_part and row[1] == 'hasType':
                outputs.append(row[2])
            elif row[0] in qualifier_msg_part and row[1] == 'hasType':
                qualifiers.append(row[2])
        
        agent_dict = {'input': inputs, 'output': outputs, 'qualifier': qualifiers}
        with open(os.path.join(self.full_dataset_dir, 'info_dict.json'), 'w') as f:
            f.write(json.dumps(agent_dict))
            f.close()
        
    def run(self):
        triples=self.get_operation_output()+\
        self.get_operation_input()+\
        self.get_resource_properties()+\
        self.get_service()+\
        self.get_http_url()+\
        self.get_operation_question_templates()

        if self.qualifier:
            triples += self.get_qualifier_for_output()

        self.write_triples_to_tsv(triples)
        MakeIndex.create_indexing(self.agent, data_dir=f'CrossGraph/{self.agent}')
        my_extractor = HopExtractor(dataset_dir=f'CrossGraph/{self.agent}', dataset_name=self.agent)
        my_creator = NegSamplingCreator(dataset_dir=f'CrossGraph/{self.agent}', ontology=self.agent)
        my_creator.create_full_neg_dictionary()
        self.create_info_dictionaries()
        
if __name__ == "__main__":
    reader1 = OntoAgentReader('ontopceagent', False, namespace='ontopceagent')
    reader2 = OntoAgentReader('ontothermoagent', True, 'ontothermoagent')
    reader1.run()
    reader2.run()