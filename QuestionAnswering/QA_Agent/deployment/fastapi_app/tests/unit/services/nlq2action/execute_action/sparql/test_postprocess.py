from typing import List, Tuple

from services.nlq2action.execute_action.sparql.link_entity import SparqlEntityLinker
from services.nlq2action.execute_action.sparql.postprocess import SparqlPostProcessor
from tests.exceptions import UnexpectedMethodCallError


class TestSparqlPostProcessor:
    def test_postprocess(self):
        class MockSparqlEntityLinker(SparqlEntityLinker):
            def __init__(self, expected_io: List[Tuple[Tuple[str], List[str]]]):
                self.expected_io = {
                    expected_input: output for expected_input, output in expected_io
                }

            def link(self, token: str):
                try:
                    return self.expected_io[(token,)]
                except:
                    raise UnexpectedMethodCallError(
                        f"Unexpected postprocessing request with token={token}"
                    )

        entity_linker = MockSparqlEntityLinker(
            [
                (
                    (
                        ('<LandUseType:"residential">',),
                        ["<http://example.org/LandUseType_0>"],
                    )
                ),
                (
                    (
                        ('<AirUseType:"recreational">',),
                        [
                            "<http://example.org/AirUseType_3>",
                        ],
                    )
                ),
                (
                    (
                        ('<AirUseType:"plane port">',),
                        [
                            "<http://example.org/AirUseType_0>",
                            "<http://example.org/AirUseType_1>",
                        ],
                    )
                ),
            ]
        )
        postprocessor = SparqlPostProcessor(entity_linker)
        sparql = """SELECT ?Plot WHERE {
    VALUES ?LandUseType { <LandUseType:"residential"> }
    ?Plot ontozoning:hasLandUseType ?LandUseType .
    VALUES ?AirUseType { <AirUseType:"plane port">  <AirUseType:"recreational"> }
    ?Plot ontozoning:hasAirUseType ?AirUseType .
}"""
        expected = """SELECT ?Plot WHERE {
    VALUES ?LandUseType { <http://example.org/LandUseType_0> }
    ?Plot ontozoning:hasLandUseType ?LandUseType .
    VALUES ?AirUseType { <http://example.org/AirUseType_0> <http://example.org/AirUseType_1> <http://example.org/AirUseType_3> }
    ?Plot ontozoning:hasAirUseType ?AirUseType .
}"""
        # Act
        actual = postprocessor.postprocess(sparql)

        # Assert
        assert actual == expected
