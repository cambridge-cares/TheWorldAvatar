from services.retrieve_docs import DocsRetriever


class TestDocsRetriever:
    def test_retrieve(self, docs_retriever: DocsRetriever):
        # Arrange
        docs = [
            "happy",
            "sad",
            "surprised",
            "melancholic",
            "joyful",
        ]
        queries = ["depressed", "contented"]
        expected = [["sad", "melancholic"], ["happy", "joyful"]]

        # Act
        retrieved = docs_retriever.retrieve(
            key="test", docs_getter=lambda: docs,
            queries=queries, k=2
        )

        # Assert
        assert len(retrieved) == 2
        actual = [[doc for doc, _ in lst] for lst in retrieved]
        print(actual)
        assert set(actual[0]) == set(expected[0])
        assert set(actual[1]) == set(expected[1])