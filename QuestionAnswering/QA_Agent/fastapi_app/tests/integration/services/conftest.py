import pytest

from redis import Redis

from services.retrieve_docs import DocsRetriever
from services.embed import TritonMPNetEmbedder


@pytest.fixture(scope="module")
def redis_client():
    client = Redis()
    yield client
    client.flushdb()

@pytest.fixture(scope="module")
def embedder():
    yield TritonMPNetEmbedder()
