import pytest
from redis import Redis
from services.retrieve_docs import DocsRetriever
from services.embed import TritonMPNetEmbedder


@pytest.fixture
def embedder():
    yield TritonMPNetEmbedder()


@pytest.fixture
def redis_client():
    client = Redis()
    yield client
    client.flushdb()


class TestDocsRetriever:
    def test_retrieve(self, embedder, redis_client):
        # Arrange
        docs_retriever = DocsRetriever(embedder, redis_client)
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
            queries=queries, key="test", docs_getter=lambda: docs, k=2
        )

        # Assert
        assert len(retrieved) == 2
        actual = [[doc for doc, _ in lst] for lst in retrieved]
        assert set(actual[0]) == set(expected[0])
        assert set(actual[1]) == set(expected[1])