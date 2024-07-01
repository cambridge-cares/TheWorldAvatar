import  pyderivationagent
from    config.a_box_updates_config import config_a_box_updates

class UpdateKG:
    """Parent class for the folder. Provides the following methods:
    - query_triple: Queries the KG based on provided triples in where clause (triple) and select statement(select_var).

        
        """


    def __init__(self, query_endpoint, update_endpoint, kg_user, kg_password):
        self.prefix  = """  PREFIX om:      <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
                            PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
                            PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
                            PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"""
        self.sparql_client          = pyderivationagent.kg_operations.PySparqlClient(
            query_endpoint          = query_endpoint,
            update_endpoint         = update_endpoint,
            kg_user                 = kg_user,
            kg_password             = kg_password
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
        return self.sparql_client.performQuery(query_triple)
    
    def query_update(self, where_stat:str, delete_stat: str, insert_stat:str):
        """takes subject, predicate and object of a tripple as input and instantiates a new triple. Please specify if object is literal => literal=True or not literal=False (Default)."""
        update_triple                = f"""
                                        DELETE  {{ {delete_stat} }}
                                        INSERT  {{ {insert_stat} }}
                                        WHERE   {{ {where_stat} }}
                                        """
        print(update_triple)
        self.sparql_client.performUpdate(update_triple)
        
    def find_all_connections(self, iri):
        """Query all incoming and outgoing connections to preserve them after they are deleted."""
        # incoming connections
        where_incoming              = f"?subject_incoming       ?predicate_incoming <{iri}> 	    ."
        select_incoming             =  "?subject_incoming       ?predicate_incoming"
        query_incoming              = self.query_triple(where_incoming, select_incoming)
        # outgoing connections
        where_outgoing              = f"""<{iri}>  	?predicate_outgoing	    ?object_outgoing	.
                                    FILTER (!isLiteral(?object_outgoing))"""
        select_outgoing             = "?predicate_outgoing ?object_outgoing"
        query_outgoing              = self.query_triple(where_outgoing, select_outgoing)
        return query_incoming, query_outgoing
    
    def delete_connections(self, iri):
        "Delete all connections to the input IRI."
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
        self.sparql_client.performUpdate(delete_incoming_query)
        self.sparql_client.performUpdate(delete_outgoing_query)
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
        self.sparql_client.performUpdate(delete_query)

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
        self.sparql_client.performUpdate(update_query)
        
    def delete_disconnected(self):
        insert_string               = f"""  ?instance rdf:type <Class> .
                                    MINUS {{ ?s ?p ?instance }}
                                    MINUS {{ ?instance ?p ?o }}"""
    def delete_overhead(self, iris, dic_string):
        for j, iri in enumerate(iris):
            subjectx                = iri[f"{dic_string}"]
            # save the IR of the first GBU-IRI
            if j == 0:
                print("-------------------------\n")
                new_subject        = iri[f"{dic_string}"]
                print(new_subject)
                print("-------------------------\n")
            # all other GBU-IRIs are redundant -> delete existing tripples and reconect with the first IRI
            else:
                self.reconnect_delete(iri[f"{dic_string}"], new_subject)

    def reconnect_delete(self, iri:str, connect_to:str):
        """Delete all incoming and outgoing triples to iri and generate new tripples from/to connect_to"""
        incoming, outgoing              = self.find_all_connections(iri)
        print(iri)
        # reconnect incoming to the i==0
        for k, am_inconnection in enumerate(incoming):
            self.generate_triple(am_inconnection["subject_incoming"],am_inconnection["predicate_incoming"], connect_to, literal=False)
        for l, am_outconnection in enumerate(outgoing):
            self.generate_triple(connect_to, am_outconnection["predicate_outgoing"], am_outconnection["object_outgoing"], literal=False)
        # delete all outgoing and incoming triples
        self.delete_connections(iri)
        return

    def run_query(self, query:str) -> list:
        return self.sparql_client.performQuery(query)
        
def main():
    a_box_updates_config        = config_a_box_updates("../../processing.env")
    updater = UpdateKG(
        query_endpoint          = a_box_updates_config.SPARQL_QUERY_ENDPOINT,
        update_endpoint         = a_box_updates_config.SPARQL_UPDATE_ENDPOINT,
        kg_user                 = a_box_updates_config.KG_USERNAME,
        kg_password             = a_box_updates_config.KG_PASSWORD
    )
    



if __name__ == "__main__":
    main()
