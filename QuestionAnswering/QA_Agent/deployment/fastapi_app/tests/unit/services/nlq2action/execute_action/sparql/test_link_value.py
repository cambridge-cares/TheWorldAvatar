from typing import List, Tuple

from services.nlq2action.execute_action.sparql.link_value import (
    SparqlEntityLinker,
)
from services.nlq2action.link_entity import ELMediator
from tests.exceptions import UnexpectedMethodCallError


class TestSparqlEntityLinker:
    def test_link(self):
        # Arrange
        class MockELMediator(ELMediator):
            def __init__(
                self, expected_io: List[Tuple[Tuple[str, str], List[Tuple[str, str]]]]
            ):
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
                        ("http://example.org/LandUseType_0", "residential 0"),
                        ("http://example.org/LandUseType_1", "residential 1"),
                    ],
                )
            ]
        )
        entity_linker = SparqlEntityLinker(el_mediator)

        # Act
        actual_values, actual_iri2label = entity_linker.link(
            '<LandUseType:"residential">'
        )

        # Assert
        assert actual_values == [
            "<http://example.org/LandUseType_0>",
            "<http://example.org/LandUseType_1>",
        ]
        assert actual_iri2label == {
            "http://example.org/LandUseType_0": "residential 0",
            "http://example.org/LandUseType_1": "residential 1",
        }
