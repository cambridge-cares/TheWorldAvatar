import pytest

from services.nlq2action.link_entity.semantic import LexiconEntry, SemanticEntityLinker
from services.nlq2action.retrieve import Nlq2ActionExample, Nlq2ActionRetriever
from services.core.retrieve_docs import DocsRetriever


@pytest.fixture(scope="class")
def semantic_entity_linker(redis_client, embedder):
    entries = [
        LexiconEntry(
            iri="https://www.theworldavatar.com/kg/landplot/LandUseType_0be686be-7ba6-410c-b7e5-ed7cb06e4590",
            label="Park",
            surface_forms=["Park", "Public Garden", "Green Space"],
        ),
        LexiconEntry(
            iri="https://www.theworldavatar.com/kg/landplot/LandUseType_1115b7ad-184a-4014-9b87-ebcc3a0eee41",
            label="Special Use",
            surface_forms=[
                "Special Use",
                "Used for Special Purposes",
                "Area with Special Purpose",
            ],
        ),
        LexiconEntry(iri="test", label="Park", surface_forms=["test"]),
    ]

    yield SemanticEntityLinker(
        redis_client=redis_client,
        embedder=embedder,
        key="landUseTypes",
        lexicon=iter(entries),
    )


class TestSemanticEntityLinker:
    def test_link(self, semantic_entity_linker: SemanticEntityLinker):
        # Arrange
        nlq = "Park"

        # Act
        actual = semantic_entity_linker.link(nlq, k=2)

        # Assert
        expected = [
            "https://www.theworldavatar.com/kg/landplot/LandUseType_0be686be-7ba6-410c-b7e5-ed7cb06e4590",
            "test",
        ]
        assert set(actual) == set(expected)
