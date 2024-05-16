import pytest

from services.entity_store import EntityStore, LexiconEntry


@pytest.fixture(scope="class")
def entity_store(redis_client, embedder):
    lexicon = [
        LexiconEntry(
            iri="http://example.org/1",
            clsname="number",
            label="one",
            surface_forms=["1", "One", "eins"],
        ),
        LexiconEntry(
            iri="http://example.org/2",
            clsname="number",
            label="two_2 (TWO-)",
            surface_forms=["two", "2", "\\Two"],
        ),
        LexiconEntry(
            iri="http://example.org/add",
            clsname="operator",
            label="addition",
            surface_forms=["plus", "add", "a_d_d"],
        ),
        LexiconEntry(
            iri="http://example.org/minus",
            clsname="operator",
            label="subtration",
            surface_forms=["subtract", "minus"],
        ),
    ]
    clsname2config = {"number": "fuzzy", "operator": "semantic"}
    yield EntityStore(
        redis_client=redis_client,
        embedder=embedder,
        lexicon=lexicon,
        clsname2elconfig=clsname2config,
    )


class TestEntityStore:
    @pytest.mark.parametrize(
        "surface_form, expected",
        [
            ("one", ["http://example.org/1"]),
            ("two_2 (TWO-)", ["http://example.org/2"]),
        ],
    )
    def test_linkExact(self, entity_store: EntityStore, surface_form, expected):
        actual = entity_store.link_exact(surface_form)
        assert actual == expected

    def test_linkSemantic(self, entity_store: EntityStore):
        # Arrange
        surface_form = "+"
        clsname = "operator"
        expected = ["http://example.org/add"]

        # Act
        actual = entity_store.link_semantic(
            surface_form=surface_form, clsname=clsname, k=1
        )

        # Assert
        assert actual == expected

    def test_linkFuzzy(self, entity_store: EntityStore):
        # Arrange
        surface_form = "one 1"
        expected = ["http://example.org/1"]

        # Act
        actual = entity_store.link_fuzzy(surface_form=surface_form, k=1)

        # Assert
        assert actual == expected
