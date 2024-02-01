from typing import List

from fastapi.testclient import TestClient
import numpy as np

from api.sparql import get_kg_executor
from api.translate import get_feature_extraction_client, get_seq2seq_client
from services.translate.triton_client.feature_extraction_client import (
    IFeatureExtractionClient,
)
from services.translate.triton_client.seq2seq_client import ISeq2SeqClient
from services.kg_execute import IKgExecutor
from main import app


client = TestClient(app)


def test_translate():
    # arrange
    def mock_seq2seq() -> ISeq2SeqClient:
        class Mock(ISeq2SeqClient):
            def forward(self, text: str):
                if text.startswith("translate to SPARQL: "):
                    return "SELECT var_s WHERE &lcub; var_s var_p var_o . &rcub;"
                elif text.startswith("classify query domain: "):
                    return "ontokin"
                else:
                    return "garbage"

        return Mock()

    app.dependency_overrides[get_seq2seq_client] = mock_seq2seq

    def mock_feature_extraction() -> IFeatureExtractionClient:
        class Mock(IFeatureExtractionClient):
            def forward(self, texts: List[str]):
                return np.random.random((len(texts), 4))

        return Mock()

    app.dependency_overrides[get_feature_extraction_client] = mock_feature_extraction

    # act
    res = client.post("/translate", json={"question": "What is the charge of benzene?"})

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
    assert expected_json.items() <= actual_json.items()
    assert "latency" in actual_json and isinstance(actual_json["latency"], float)


def test_sparql():
    # arrange
    def mock_kg() -> IKgExecutor:
        class MockKgExecutor(IKgExecutor):
            def query(self, domain: str, query: str):
                return {"results": {"bindings": [{"var1": {"value": "1"}}]}}

        return MockKgExecutor()

    app.dependency_overrides[get_kg_executor] = mock_kg

    # act
    res = client.post(
        "/sparql", json={"query": "SELECT * WHERE {}", "domain": "chemistry"}
    )

    # assert
    assert res.status_code == 200
    expected_json = {"data": {"results": {"bindings": [{"var1": {"value": "1"}}]}}}
    actual_json = res.json()
    assert isinstance(actual_json, dict)
    assert expected_json.items() <= actual_json.items()
    assert "latency" in actual_json and isinstance(actual_json["latency"], float)
