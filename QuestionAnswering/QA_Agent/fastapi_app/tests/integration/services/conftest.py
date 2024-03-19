import os
import pytest

from redis import Redis
from yarl import URL

from services.embed import TritonMPNetEmbedder


@pytest.fixture(scope="module")
def redis_client():
    client = Redis(os.getenv("TEST_REDIS_HOST", "localhost"))
    yield client
    client.flushdb()


@pytest.fixture(scope="module")
def embedder():
    yield TritonMPNetEmbedder(os.getenv("TEST_TRITON_URL", "localhost:8001"))


@pytest.fixture(scope="module")
def blazegraph_base_url():
    yield URL(os.getenv("TEST_BLAZEGRAPH_BASE_URL", "http://localhost:9999"))


