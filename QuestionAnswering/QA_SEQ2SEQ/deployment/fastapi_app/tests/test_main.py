from collections import defaultdict
from typing import List

from fastapi.testclient import TestClient
import numpy as np

from api.sparql import get_domain2endpoint, get_kg_client_factory
from api.translate import get_feature_extraction_client, get_seq2seq_client
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
            def forward(self, text: str):
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
