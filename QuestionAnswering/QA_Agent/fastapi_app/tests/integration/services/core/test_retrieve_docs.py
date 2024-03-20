from services.core.retrieve_docs import DocsRetriever


class TestDocsRetriever:
    def test_retrieve(self, redis_client, embedder):
        # Arrange
        docs = [
            "happy",
            "sad",
            "surprised",
            "melancholic",
            "joyful",
        ]
        docs_retriever = DocsRetriever(
            embedder=embedder, redis_client=redis_client, key="test", docs=iter(docs)
        )
        queries = ["depressed", "contented"]

        # Act
        retrieved = docs_retriever.retrieve(queries=queries, k=2)

        # Assert
        expected = [["sad", "melancholic"], ["happy", "joyful"]]
        assert len(retrieved) == len(expected)

        actual = [[doc for doc, _ in lst] for lst in retrieved]
        assert set(actual[0]) == set(expected[0])
        assert set(actual[1]) == set(expected[1])
