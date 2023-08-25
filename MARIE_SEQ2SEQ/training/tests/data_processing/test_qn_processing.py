import pytest
from core.data_processing.qn_processing import preprocess_qn


class TestQnProcessing:
    @pytest.mark.parametrize(
            "qn, model_family, expected", [
                ("What is the molecular weight of ethanol?", "t5", "translate to SPARQL: What is the molecular weight of ethanol?"),
                ("What is the molecular weight of ethanol?", "llama", "translate to SPARQL: What is the molecular weight of ethanol?\n\n###\n\n")
            ]
    )
    def test_preprocessQn(self, qn, model_family, expected):
        assert preprocess_qn(qn, model_family=model_family) == expected