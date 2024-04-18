import pytest
from services.core.retrieve_docs import DocsRetriever


@pytest.fixture(scope="class")
def docs_retriever(redis_client, embedder):
    docs = [
        "happy",
        "sad",
        "surprised",
        "melancholic",
        "joyful",
    ]

    yield DocsRetriever(
        embedder=embedder, redis_client=redis_client, key="test", docs=iter(docs)
    )


@pytest.fixture(scope="class")
def docs_retriever_with_tags(redis_client, embedder):
    docs = [
        "happy",
        "sad",
        "surprised",
        "joyful",
        "melancholic",
    ]
    tags = ["a", "b", "a", "b", "a"]

    yield DocsRetriever(
        embedder=embedder,
        redis_client=redis_client,
        key="test_with_keys",
        docs=iter(docs),
        tags=iter(tags),
    )


class TestDocsRetriever:
    def test_retrieve(self, docs_retriever: DocsRetriever[str]):
        # Arrange
        queries = ["depressed", "contented"]

        # Act
        retrieved = docs_retriever.retrieve(queries=queries, k=2)

        # Assert
        expected = [["sad", "melancholic"], ["happy", "joyful"]]
        assert len(retrieved) == len(expected)

        actual = [[doc for doc, _ in lst] for lst in retrieved]
        assert set(actual[0]) == set(expected[0])
        assert set(actual[1]) == set(expected[1])

    def test_match(self, docs_retriever: DocsRetriever[str]):
        # Act
        match = docs_retriever.match("happiness")

        # Assert
        assert match == "happy"

    def test_retrieve_withTag(self, docs_retriever_with_tags: DocsRetriever[str]):
        # Act
        match, _ = docs_retriever_with_tags.retrieve(
            queries=["happiness"], k=1, tag="b"
        )[0][0]

        # Assert
        assert match == "joyful"
