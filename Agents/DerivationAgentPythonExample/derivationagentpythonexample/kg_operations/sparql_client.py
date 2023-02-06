# First import PySparqlClient as the base class for your sparql client to inherit from
from typing import Optional
from pyderivationagent.kg_operations.sparql_client import PySparqlClient

# Then import the data models from your project
import derivationagentpythonexample.data_model as dm

# Also import all other third party libraries you need
from rdflib import Literal
from rdflib import URIRef
from rdflib import Graph
from rdflib import RDF
import uuid

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

class ExampleSparqlClient(PySparqlClient):
    # NOTE __init__ is already defined in the base class, so you don't need to define it again

    ###################################
    ## Define your own methods below ##
    ###################################

    def get_max_value(self, iri: str) -> Optional[dm.MaxValue]:
        """Query the knowledge graph for the given MaxValue instance IRI and return it as a MaxValue object.

        Args:
            iri (str): The IRI of the MaxValue instance

        Raises:
            Exception: There should only be one instance of Value for the given MaxValue, but there are multiple or none

        Returns:
            Optional[dm.MaxValue]: The MaxValue instance as a MaxValue object
        """
        query = f"""
            SELECT ?value ?num_val
            WHERE {{
                <{iri}> <{dm.DERIVATION_AGENT_PYTHON_EXAMPLE_HASVALUE}> ?value .
                ?value <{dm.DERIVATION_AGENT_PYTHON_EXAMPLE_NUMVAL}> ?num_val .
            }}"""
        response = self.performQuery(query)

        if len(response) == 1:
            return dm.MaxValue(
                instance_iri=iri,
                hasValue=dm.Value(instance_iri=response[0]['value'], numVal=response[0]['num_val']),
            )
        else:
            raise Exception(f"There should only be one instance of Value for MaxValue {iri}, however, found: {response}")

    def get_min_value(self, iri: str) -> Optional[dm.MinValue]:
        """Query the knowledge graph for the given MinValue instance IRI and return it as a MinValue object.

        Args:
            iri (str): The IRI of the MinValue instance

        Raises:
            Exception: There should only be one instance of Value for the given MinValue, but there are multiple or none

        Returns:
            Optional[dm.MinValue]: The MinValue instance as a MinValue object
        """
        query = f"""
            SELECT ?value ?num_val
            WHERE {{
                <{iri}> <{dm.DERIVATION_AGENT_PYTHON_EXAMPLE_HASVALUE}> ?value .
                ?value <{dm.DERIVATION_AGENT_PYTHON_EXAMPLE_NUMVAL}> ?num_val .
            }}"""
        response = self.performQuery(query)

        if len(response) == 1:
            return dm.MinValue(
                instance_iri=iri,
                hasValue=dm.Value(instance_iri=response[0]['value'], numVal=response[0]['num_val']),
            )
        else:
            raise Exception(f"There should only be one instance of Value for MinValue {iri}, however, found: {response}")

    def get_difference(self, iri: str) -> Optional[dm.Difference]:
        query = f"""
            SELECT ?value ?num_val
            WHERE {{
                <{iri}> <{dm.DERIVATION_AGENT_PYTHON_EXAMPLE_HASVALUE}> ?value .
                ?value <{dm.DERIVATION_AGENT_PYTHON_EXAMPLE_NUMVAL}> ?num_val .
            }}"""
        response = self.performQuery(query)

        if len(response) == 1:
            return dm.Difference(
                instance_iri=iri,
                hasValue=dm.Value(instance_iri=response[0]['value'], numVal=response[0]['num_val']),
            )
        else:
            raise Exception(f"There should only be one instance of Value for Difference {iri}, however, found: {response}")

    def create_difference(self, diff_num_val: float) -> Graph:
        """Create a new Difference instance and return it as a rdflib.Graph, which will then be added to derivation_outputs.

        Args:
            diff_num_val (float): The num_val of the new Difference instance

        Returns:
            Graph: The new Difference instance as a rdflib.Graph
        """
        g = Graph()
        diff_iri = dm.DERIVATION_AGENT_PYTHON_EXAMPLE_BASE_URL + 'Difference_' + str(uuid.uuid4())
        value_iri = dm.DERIVATION_AGENT_PYTHON_EXAMPLE_BASE_URL + 'Value_' + str(uuid.uuid4())
        # Add the triples to the graph
        g.add((URIRef(diff_iri), RDF.type, URIRef(dm.DERIVATION_AGENT_PYTHON_EXAMPLE_DIFFERENCE)))
        g.add((URIRef(diff_iri), URIRef(dm.DERIVATION_AGENT_PYTHON_EXAMPLE_HASVALUE), URIRef(value_iri)))
        g.add((URIRef(value_iri), RDF.type, URIRef(dm.DERIVATION_AGENT_PYTHON_EXAMPLE_VALUE)))
        g.add((URIRef(value_iri), URIRef(dm.DERIVATION_AGENT_PYTHON_EXAMPLE_NUMVAL), Literal(diff_num_val)))
        return g
