import json
from typing import List

from fastapi.testclient import TestClient
from httpx import AsyncClient
import numpy as np
from openai import OpenAI, resources
from openai.types.chat.chat_completion_chunk import (
    ChatCompletionChunk,
    Choice,
    ChoiceDelta,
)
import pytest

from api.sparql import get_domain2endpoint, get_kg_client_factory
from api.translate import get_feature_extraction_client, get_seq2seq_client
from api.chat import get_openai_client, get_openai_config, get_tokens_counter
from services.chatbot import OpenAiConfig
from services.kg_execute.kg_client import KgClient
from services.translate.triton_client.feature_extraction_client import (
    IFeatureExtractionClient,
)
from services.translate.triton_client.seq2seq_client import ISeq2SeqClient
from main import app


client = TestClient(app)


def test_translate():
    # arrange
    def mock_seq2seq():
        class Mock(ISeq2SeqClient):
            def forward(self, text: str, model: str):
                if text.startswith("translate to SPARQL: "):
                    return "SELECT var_s WHERE &lcub; var_s var_p var_o . &rcub;"
                elif text.startswith("classify query domain: "):
                    return "ontokin"
                else:
                    return "garbage"

        return Mock()

    app.dependency_overrides[get_seq2seq_client] = mock_seq2seq

    def mock_feature_extraction():
        class Mock(IFeatureExtractionClient):
            def forward(self, texts: List[str]):
                return np.random.random((len(texts), 4))

        return Mock()

    app.dependency_overrides[get_feature_extraction_client] = mock_feature_extraction

    # act
    res = client.post(
        "/translate",
        json={"question": "What is the charge of benzene?", "domain": None},
    )

    # assert
    assert res.status_code == 200
    expected_json = {
        "question": "What is the charge of benzene?",
        "preprocessed_question": "What is the charge of benzene?",
        "domain": "ontokin",
        "sparql": {
            "predicted": "SELECT ?s WHERE { ?s ?p ?o . }",
            "postprocessed": "SELECT DISTINCT ?s WHERE {\n  ?s ?p ?o .\n}",
        },
    }
    actual_json = res.json()
    assert isinstance(actual_json, dict)
    assert expected_json.items() <= actual_json.items(), print(actual_json)
    assert "latency" in actual_json and isinstance(actual_json["latency"], float)


def test_sparql():
    # arrange
    def mock_domain2endpoint():
        return {"test_domain": "test_endpoint"}

    app.dependency_overrides[get_domain2endpoint] = mock_domain2endpoint

    def mock_kg_client_factory():
        class Mock(KgClient):
            def __init__(self, endpoint):
                pass

            def query(self, query: str):
                return {"results": {"bindings": [{"var1": {"value": "1"}}]}}

        return Mock

    app.dependency_overrides[get_kg_client_factory] = mock_kg_client_factory

    # act
    res = client.post(
        "/sparql", json={"query": "SELECT * WHERE {}", "domain": "test_domain"}
    )

    # assert
    assert res.status_code == 200, res
    expected_json = {"data": {"results": {"bindings": [{"var1": {"value": "1"}}]}}}
    actual_json = res.json()
    assert isinstance(actual_json, dict)
    assert expected_json.items() <= actual_json.items()
    assert "latency" in actual_json and isinstance(actual_json["latency"], float)


@pytest.mark.asyncio
async def test_chat():
    # arrange
    def mock_openai_config():
        return OpenAiConfig(model="test_model", input_limit=1000)

    app.dependency_overrides[get_openai_config] = mock_openai_config

    def mock_openai_client():
        class MockCompletions(resources.Completions):
            def __init__(self):
                pass

            def create(self, stream: bool, **kwargs):
                assert stream
                for c in "This is chatbot's response":
                    yield ChatCompletionChunk(
                        id="test_id",
                        choices=[
                            Choice(
                                delta=ChoiceDelta(content=c),
                                finish_reason=None,
                                index=0,
                            )
                        ],
                        created=0,
                        model="test_model",
                        object="chat.completion.chunk",
                    )

        class MockChat(resources.Chat):
            def __init__(self):
                self.completions = MockCompletions()

        class MockOpenAI(OpenAI):
            def __init__(self):
                self.chat = MockChat()

        return MockOpenAI()

    app.dependency_overrides[get_openai_client] = mock_openai_client

    def mock_tokens_counter():
        def count(text: str):
            return len(text.split()) * 4 // 3

        return count

    app.dependency_overrides[get_tokens_counter] = mock_tokens_counter

    # act
    async with AsyncClient(app=app, base_url="http://test") as client:
        res = await client.post(
            "/chat",
            json={
                "question": "What is the charge of benzene?",
                "data": '[{"Charge":"0.0"}]',
            },
        )

    # assert
    assert res.status_code == 200
    content = res.content.decode("utf-8")
    lines = [line for line in content.split("\n") if line]
    assert all(line.startswith("data: ") for line in lines)
    lines = [json.loads(line[len("data: ") :]) for line in lines]
    assert all("content" in line and "latency" in line for line in lines)
    assert "".join([line["content"] for line in lines]) == "This is chatbot's response"
