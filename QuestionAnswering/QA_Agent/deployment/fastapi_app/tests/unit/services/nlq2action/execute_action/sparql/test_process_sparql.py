from typing import List, Optional, Tuple


from services.entity_store import EntityStore
from services.nlq2action.execute_action.sparql.process_sparql import SparqlProcessor
from tests.exceptions import UnexpectedMethodCallError


class TestSparqlProcessor:
    class MockEntityStore(EntityStore):
        def __init__(
            self,
            link_expected_io: List[
                Tuple[Tuple[str, Optional[str], int], List[str]]
            ] = dict(),
        ):
            self.link_expected_io = {
                expected_input: output for expected_input, output in link_expected_io
            }

        def link(self, surface_form: str, clsname: Optional[str] = None, k: int = 3):
            try:
                return self.link_expected_io[(surface_form, clsname, k)]
            except Exception as e:
                print(e)
                raise UnexpectedMethodCallError(
                    f"Unexpected postprocessing request with surface_form={surface_form}, clsname={clsname}, k={k}"
                )

    def test_linkEntities(self):
        # Arrange
        entity_linker = self.MockEntityStore(
            [
                (
                    ("residential", "LandUseType", 3),
                    ["http://example.org/LandUseType_0"],
                ),
                (
                    ("recreational", "AirUseType", 3),
                    ["http://example.org/AirUseType_3"],
                ),
                (
                    ("plane port", "AirUseType", 3),
                    [
                        "http://example.org/AirUseType_0",
                        "http://example.org/AirUseType_1",
                    ],
                ),
            ]
        )
        processor = SparqlProcessor(entity_linker)

        sparql_in = """SELECT ?LandUseType ?AirUseType ?Plot WHERE {
    VALUES ?LandUseType { <LandUseType:"residential"> }
    ?Plot ontozoning:hasLandUseType ?LandUseType .
    VALUES ?AirUseType { <AirUseType:"plane port">  <AirUseType:"recreational"> }
    ?Plot ontozoning:hasAirUseType ?AirUseType .
}"""
        expected_sparql = """SELECT ?LandUseType ?AirUseType ?Plot WHERE {
    VALUES ?LandUseType { <http://example.org/LandUseType_0> }
    ?Plot ontozoning:hasLandUseType ?LandUseType .
    VALUES ?AirUseType { <http://example.org/AirUseType_0> <http://example.org/AirUseType_1> <http://example.org/AirUseType_3> }
    ?Plot ontozoning:hasAirUseType ?AirUseType .
}"""
        expected_varnames = ["LandUseType", "AirUseType"]

        # Act
        actual_sparql, actual_varnames = processor.link_entities(sparql_in)

        # Assert
        assert actual_sparql == expected_sparql
        assert actual_varnames == expected_varnames

    def test_injectServiceEndpoint(self):
        # Arrange
        entity_store = self.MockEntityStore()
        ns2uri = {
            "ontop": "http://example.org/ontop/sparql",
            "bg": "http://example.come/blazegraph/sparql",
        }
        processor = SparqlProcessor(entity_store=entity_store, ns2uri=ns2uri)

        sparql_in = """SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
    SERVICE <bg> {
        ?o ?p ?s .
    }
    SERVICE <ontop> {
        ?p ?s ?o .
    }
}"""
        expected = """SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
    SERVICE <http://example.come/blazegraph/sparql> {
        ?o ?p ?s .
    }
    SERVICE <http://example.org/ontop/sparql> {
        ?p ?s ?o .
    }
}"""

        # Act
        actual = processor.inject_service_endpoint(sparql_in)

        # Assert
        assert actual == expected
