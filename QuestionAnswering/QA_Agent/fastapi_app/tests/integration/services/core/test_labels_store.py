from services.core.labels_store import IRIWithLabels, LabelsStore


class TestLabelsStore:
    SAMPLE_BINDINGS = [
        IRIWithLabels(IRI="1", labels=["one", "One"]),
        IRIWithLabels(IRI="2", labels=["two", "tWo"]),
        IRIWithLabels(IRI="3", labels=["thRee", "3333"]),
        IRIWithLabels(IRI="3bis", labels=["thRee"])
    ]

    def test_linkEntity(self, redis_client):
        # Arrange
        labels_store = LabelsStore(
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
        labels_store = LabelsStore(
            redis_client=redis_client,
            key_prefix="test:",
            index_name="idx:test",
            bindings=self.SAMPLE_BINDINGS,
        )

        # Act
        actual = labels_store.lookup_labels("2")

        # Assert
        expected = ["two", "tWo"]
        actual == expected
