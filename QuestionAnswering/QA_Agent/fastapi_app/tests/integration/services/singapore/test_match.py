import os

import pytest
from redis import Redis

from services.embed import TritonMPNetEmbedder
from services.kg_client import KgClient
from services.connector.singapore.match import LandUseTypeMatcher

@pytest.fixture
def redis_client():
    client = Redis()
    yield client
    client.flushdb()
    
class TestLandUseTypeMatcher:
    @pytest.mark.parametrize(
            "query,expected",[
                ("park", "https://www.theworldavatar.com/kg/landplot/LandUseType_0be686be-7ba6-410c-b7e5-ed7cb06e4590"),
                ("airport", "https://www.theworldavatar.com/kg/landplot/LandUseType_80755ccd-8068-4446-b40c-ee064b0fae56"),
                ("religious buildings", "https://www.theworldavatar.com/kg/landplot/LandUseType_de2f9725-4360-4b0d-b237-fb71b7f09201")
            ]
    )
    def test_match(self, redis_client, query, expected):
        # Arrange
        kg_client = KgClient(os.getenv("KG_ENDPOINT_SINGAPORE"))
        embedder = TritonMPNetEmbedder()
        matcher = LandUseTypeMatcher(kg_client=kg_client, embedder=embedder, redis_client=redis_client)

        # Act
        actual = matcher.match(query)

        # Assert
        assert actual[0] == expected