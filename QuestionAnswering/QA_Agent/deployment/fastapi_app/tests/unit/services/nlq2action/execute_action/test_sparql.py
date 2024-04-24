from typing import List, Tuple

from services.nlq2action.execute_action.sparql import (
    SparqlEntityLinker,
    SparqlPostProcessor,
)
from services.nlq2action.link_entity import ELMediator
from tests.exceptions import UnexpectedMethodCallError


class TestSparqlEntityLinker:
    def test_link(self):
        # Arrange
        class MockELMediator(ELMediator):
            def __init__(self, expected_io: List[Tuple[Tuple[str, str], List[str]]]):
                self.expected_io = {
                    expected_input: output for expected_input, output in expected_io
                }

            def link(self, entity_type: str, surface_form: str):
                try:
                    return self.expected_io[(entity_type, surface_form)]
                except:
                    raise UnexpectedMethodCallError(
                        f"Unexpected EL request with entity_type={entity_type} and surface_form={surface_form}"
                    )

        el_mediator = MockELMediator(
            [
                (
                    ("LandUseType", "residential"),
                    [
                        "http://example.org/LandUseType_0",
                        "http://example.org/LandUseType_1",
                    ],
                )
            ]
        )
        entity_linker = SparqlEntityLinker(el_mediator)

        # Act
        actual = entity_linker.link('<LandUseType:"residential">')

        # Assert
        assert actual == [
            "<http://example.org/LandUseType_0>",
            "<http://example.org/LandUseType_1>",
        ]


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
                        ('<AirUseType:"port">',),
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
    VALUES ?AirUseType { <AirUseType:"port">  <AirUseType:"recreational"> }
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
