from services.core.label_store import IRIWithLabels, LabelStore


class TestLabelStore:
    SAMPLE_BINDINGS = [
        IRIWithLabels(IRI="1", labels=["one", "One"]),
        IRIWithLabels(IRI="http://test.org/2", labels=["two", "tWo"]),
        IRIWithLabels(IRI="3", labels=["thRee", "3333"]),
        IRIWithLabels(IRI="3bis", labels=["thRee"])
    ]

    def test_linkEntity(self, redis_client):
        # Arrange
        labels_store = LabelStore(
            redis_client=redis_client,
            key_prefix="test:",
            index_name="idx:test",
            bindings=self.SAMPLE_BINDINGS,
        )

        # Act
        actual = labels_store.link_entity("three")

        # Assert
        expected = ["3", "3bis"]
        assert set(actual) == set(expected)

    def test_lookupLabels(self, redis_client):
        # Arrange
        labels_store = LabelStore(
            redis_client=redis_client,
            key_prefix="test:",
            index_name="idx:test",
            bindings=self.SAMPLE_BINDINGS,
        )

        # Act
        actual = labels_store.lookup_labels("http://test.org/2")

        # Assert
        expected = ["two", "tWo"]
        actual == expected
