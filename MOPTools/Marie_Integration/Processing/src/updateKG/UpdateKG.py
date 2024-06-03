import pyderivationagent

class UpdateKG:
    """Parent class for the folder."""


    def __init__(self, query_endpoint, update_endpoint, kg_user, kg_password):
        self.prefix  = """PREFIX om:      <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
                 PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
                 PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>"""
        self.sparql_client      = pyderivationagent.kg_operations.PySparqlClient(
            query_endpoint      = query_endpoint,
            update_endpoint     = update_endpoint,
            kg_user             = kg_user,
            kg_password         = kg_password
        )


         
    def find_all_connections(self, iri):
        """Query all incoming and outgoing connections to preserve them after they are deleted."""
        query_incoming = f"""
        {self.prefix}
        SELECT ?subject_incoming ?predicate_incoming
        WHERE {{
        ?subject_incoming    ?predicate_incoming <{iri}> 	    .
            }}
        """
        query_outgoing = f"""
        {self.prefix}
        SELECT ?predicate_outgoing ?object_outgoing
        WHERE {{
        <{iri}>  	?predicate_outgoing	    ?object_outgoing	    .
        FILTER (!isLiteral(?object_outgoing))
        }}
        """
        # query
        incoming_iris   = self.sparql_client.performQuery(query_incoming)
        outgoing_iris   = self.sparql_client.performQuery(query_outgoing)
        print(f"The IRI: {iri} is has outgoing connections to {outgoing_iris} and incoming connections {incoming_iris}")
        return incoming_iris, outgoing_iris
    
    def delete_connections(self, iri):
        "Delete all connections to the input IRI."
        delete_incoming_query       = f"""
        {self.prefix}
        DELETE DATA {{
        ?incomingIRI ?incomingP <{iri}> .
        }}
        """
        delete_outgoing_query       = f"""
        {self.prefix}
        DELETE DATA {{
        <{iri}> ?outgoingP ?outgoingIRI .
        }}
        """

    def generate_triple(self, subject, predicate, object):
        """takes subject, predicate and object of a tripple as input and instantiates a new triple. Requires all of them to be IRIs!"""
        update_query        = f"""
        {self.prefix}
        INSERT DATA {{
            <{subject}> <{predicate}> <{object}> .
        }}
        """


def main():
    SPARQL_QUERY_ENDPOINT       = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    SPARQL_UPDATE_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    KG_USERNAME                 = 'bg_user'
    KG_PASSWORD                 = 'admin'
    FILE_DIRECTORY              = "../data/MOPGeometryData"

    updater = UpdateKG(
        query_endpoint          = SPARQL_QUERY_ENDPOINT,
        update_endpoint         = SPARQL_UPDATE_ENDPOINT,
        kg_user                 = KG_USERNAME,
        kg_password             = KG_PASSWORD
    )
    



if __name__ == "__main__":
    main()
