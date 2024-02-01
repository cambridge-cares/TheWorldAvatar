from fastapi.testclient import TestClient

from api.translate import get_translator
from api.sparql import get_kg_executor
from services.kg_execute import IKgExecutor
from services.translate import ITranslator, TranslateResult, TranslateResultSparql
from main import app


client = TestClient(app)


def test_translate():
    # arrange
    def mock_translator() -> ITranslator:
        class MockTranslator(ITranslator):
            def nl2sparql(self, question: str):
                return TranslateResult(
                    domain="chemistry",
                    sparql=TranslateResultSparql(
                        raw="RawSparql",
                        decoded="DecodedSparql",
                        verbose="VerboseSparql",
                    ),
                )

        return MockTranslator()

    app.dependency_overrides[get_translator] = mock_translator

    # act
    res = client.post("/translate", json={"question": "What is the charge of benzene?"})

    # assert
    assert res.status_code == 200
    expected_json = {
        "question": "What is the charge of benzene?",
        "preprocessed_question": "What is the charge of benzene?",
        "domain": "chemistry",
        "sparql": {"predicted": "DecodedSparql", "postprocessed": "VerboseSparql"},
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
