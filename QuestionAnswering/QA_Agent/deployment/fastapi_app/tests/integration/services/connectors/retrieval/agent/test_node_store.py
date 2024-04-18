import pytest
from yarl import URL

from services.connectors.retrieval.agent.node_store import NodeStore
from services.core.kg import KgClient
from tests.integration.services.utils import TriplesManager


@pytest.fixture(scope="class")
def retrieval_kgClient(blazegraph_base_url: URL):
    triples_manager = TriplesManager(
        base_url=blazegraph_base_url,
        namespace="retrieval",
        prefixes="""PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontozoning: <https://www.theworldavatar.com/kg/ontozoning/>
PREFIX ontoplanningregulation: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX landplot: <https://www.theworldavatar.com/kg/landplot/>""",
    )

    triples_manager.insert(
        """landplot:LandUseType_de2f9725-4360-4b0d-b237-fb71b7f09201
    rdf:type ontozoning:PlaceOfWorship ;
    rdfs:comment "These are areas used or intended to be used mainly for religious buildings."^^xsd:string ;
    rdfs:label "Place of Worship"^^xsd:string .
landplot:LandUseRegulation_1df039e9-9438-4dec-988a-8784d93d8502 
    rdfs:label "Place of Worship Development Control Handbook"^^xsd:string ;
    ontoplanningregulation:appliesTo landplot:LandUseType_de2f9725-4360-4b0d-b237-fb71b7f09201 ;
    rdf:type ontoplanningregulation:LandUseRegulation ."""
    )

    endpoint = str(blazegraph_base_url / "blazegraph/namespace/retrieval/sparql")
    yield KgClient(endpoint)

    triples_manager.delete_all()


@pytest.fixture(scope="class")
def node_store(retrieval_kgClient):
    yield NodeStore(retrieval_kgClient)


class TestNodeStore:
    def test_triplesGen(self, node_store: NodeStore):
        # Arrange
        expected = [
            """Place of Worship type PlaceOfWorship
Place of Worship comment These are areas used or intended to be used mainly for religious buildings.
Place of Worship label Place of Worship
Place of Worship Development Control Handbook appliesTo Place of Worship""",
            """Place of Worship Development Control Handbook appliesTo Place of Worship
Place of Worship Development Control Handbook label Place of Worship Development Control Handbook
Place of Worship Development Control Handbook type LandUseRegulation""",
        ]
        expected = set([tuple(sorted(x.split("\n"))) for x in expected])

        # Act
        actual = [NodeStore.linearize_doc(x) for x in node_store.triples_gen()]
        actual = set([tuple(sorted(x.split("\n"))) for x in actual])

        # Assert
        assert expected == actual
