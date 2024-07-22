from twa.kg_operations          import PySparqlClient
from rdflib                     import Graph, URIRef, Literal
from rdflib.namespace           import RDF
import os
import sys
import uuid
PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)
from rework_ontomops.update_kg  import config_a_box_updates
import json
import pandas as pd

def read_json_file(file_path):
    """
    Reads a JSON file and returns the data as a dictionary.

    Args:
    file_path (str): The path to the JSON file.

    Returns:
    dict: The data parsed from the JSON file.
    """
    with open(file_path, 'r') as file:
        #data        = json.load(file)
        data2           = pd.read_json(file)
        # Set display options to show the full DataFrame
        pd.set_option('display.max_columns', None)
        pd.set_option('display.max_rows', None)
        pd.set_option('display.max_colwidth', None)
        pd.set_option('display.width', None)
        data2           = data2.at[0, 'Synthesis']
    return data2




class UploadKG:
    """Parent class for the folder. Provides the following methods:
    - query_triple: Queries the KG based on provided triples in where clause (triple) and select statement(select_var). 
    """

    def __init__(self, query_endpoint, update_endpoint, kg_user, kg_password):
        self.prefix                 = """  PREFIX om:      <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
                                           PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
                                           PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
                                           PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"""
        a_box_updates_config        = config_a_box_updates("../OntoSynthesisConnection.env")
        self.sparql_client          = PySparqlClient(
        query_endpoint              = a_box_updates_config.SPARQL_QUERY_ENDPOINT    ,
        update_endpoint             = a_box_updates_config.SPARQL_UPDATE_ENDPOINT   ,
        kg_user                     = a_box_updates_config.KG_USERNAME              ,
        kg_password                 = a_box_updates_config.KG_PASSWORD              ,
        fs_url                      = ""                                            ,
        fs_user                     = ""                                            ,
        fs_pwd                      = ""
        )

    def query_triple(self, triple, select_var):
        """Queries the KG based on provided triples in where clause (triple) and select statement(select_var).
        Inputs: 
            - triple:     String in where clause.
            - select_var: String with return variables
        Outputs:
        """
        query_triple                = f"""
                                        {self.prefix}
                                        SELECT {select_var}
                                        WHERE {{
                                        {triple}
                                            }}
                                        """
        # query
        return self.sparql_client.perform_query(query_triple)
    
    def query_update(self, where_stat:str, delete_stat: str, insert_stat:str):
        """takes subject, predicate and object of a tripple as input and instantiates a new triple. Please specify if object is literal => literal=True or not literal=False (Default)."""
        update_triple                = f"""
                                        DELETE  {{ {delete_stat} }}
                                        INSERT  {{ {insert_stat} }}
                                        WHERE   {{ {where_stat} }}
                                        """
        print(update_triple)
        self.sparql_client.perform_update(update_triple)
        
    
    def delete_connections(self, iri):
        "Delete all connections from and to the input IRI."
        delete_incoming_query       = f"""
        {self.prefix}
        DELETE WHERE {{
        ?incomingIRI ?incomingP <{iri}> .
        }}
        """
        delete_outgoing_query       = f"""
        {self.prefix}
        DELETE WHERE {{
        <{iri}> ?outgoingP ?outgoingIRI .
        }}
        """
        self.sparql_client.perform_update(delete_incoming_query)
        self.sparql_client.perform_update(delete_outgoing_query)
        print(f"deleted outgoing: {delete_incoming_query}")

    def delete_triple(self, tsubject, tpredicate, tobject, literal=False):
        "Delete all connections to the input IRI."
        if literal:
            insert_string           = f"""<{tsubject}> <{tpredicate}> "{tobject}" ."""
        else:
            insert_string           = f"""<{tsubject}> <{tpredicate}> <{tobject}> ."""
        delete_query                = f"""
        {self.prefix}
        DELETE DATA {{
        {insert_string}
        }}
        """
        print(f"deleted triple: <{tsubject}>, <{tpredicate}>, {tobject}")
        self.sparql_client.perform_update(delete_query)

    def generate_triple(self, tsubject, tpredicate, tobject, literal=False):
        """takes subject, predicate and object of a tripple as input and instantiates a new triple. Please specify if object is literal => literal=True or not literal=False (Default)."""
        if literal:
            insert_string           = f"""<{tsubject}> <{tpredicate}> "{tobject}" ."""
        else:
            insert_string           = f"""<{tsubject}> <{tpredicate}> <{tobject}> ."""
        print(f"generated tripple: {insert_string}.")
        update_query                = f"""
        {self.prefix}
        INSERT DATA {{
            {insert_string}
        }}
        """
        self.sparql_client.perform_update(update_query)
        
def upload_procedure():
    return


        
def main():
    a_box_updates_config        = config_a_box_updates("../OntoSynthesisConnection.env")
    updater                     = UploadKG(
        query_endpoint          = a_box_updates_config.SPARQL_QUERY_ENDPOINT,
        update_endpoint         = a_box_updates_config.SPARQL_UPDATE_ENDPOINT,
        kg_user                 = a_box_updates_config.KG_USERNAME,
        kg_password             = a_box_updates_config.KG_PASSWORD
    )
    triple                      = f"""  
    ?MOPIRI     om:hasMOPFormula 	        ?MOPFormula						;
                om:hasChemicalBuildingUnit	?CBUIRI				            ;
                om:hasProvenance 	        ?Provenance 					;
                om:hasCCDCNumber	        ?CCDC							.
    ?Provenance om:hasReferenceDOI          "10.1002/chem.201700798" 	    ."""
    select_stat                 = """?MOPFormula ?CCDC"""
    print(updater.query_triple(triple=triple, select_var=select_stat))
    data = read_json_file("../Data/first10_prompt5/10.1021_ja 0104352.json")
    print(data)
    # Convert main elements to a DataFrame
    main_df         = pd.DataFrame({k: [v] for k, v in data.items() if not isinstance(v, list)})

    # Print main DataFrame
    print(main_df)

    # Convert nested lists to DataFrames
    add_df          = pd.DataFrame(data['Add'])
    heatchill_df    = pd.DataFrame(data['HeatChill'])
    filter_df       = pd.DataFrame(data['Filter'])
    stirr_df        = pd.DataFrame(data['Stirr'])
    sonicate_df     = pd.DataFrame(data['Sonicate'])

    OntoSyn         = "http://www.theworldavatar.com/ontology/ontosyn/OntoSyn.owl"

    # Print nested DataFrames
    print("\nAdd DataFrame:")
    print(add_df)

    print("\nHeatChill DataFrame:")
    print(heatchill_df)

    print("\nFilter DataFrame:")
    print(filter_df)

    # Access subelements
    # Example: Access the 'added chemical name' from the 'Add' DataFrame
    added_chemical_names        = add_df['added chemical name']
    print("\nAdded Chemical Names:")
    print(added_chemical_names)
    # instantiate rdflib.Graph object
    g                           = Graph()
    hash_id                     = str(uuid.uuid4())
    insert_statement            = f"""  <https://www.theworldavatar.com/kg/ontosyn/OntoSyn.owl#ChemicalSynthesis_{hash_id}>       
                                        <https://www.theworldavatar.com/kg/ontosyn/OntoSyn.owl#hasSynthesisStep>    	
                                        <https://www.theworldavatar.com/kg/ontosyn/OntoSyn.owl#Add_{hash_id}>	            .  """
    updater.query_update("", "", insert_statement)


if __name__ == "__main__":
    main()

