import pytest
from core.data_processing.input_processing import preprocess_input


class TestInputProcessing:
    @pytest.mark.parametrize(
            "qn, model_family, expected", [
                ("What is the molecular weight of ethanol?", "t5", "translate to SPARQL: What is the molecular weight of ethanol?"),
                ("What is the molecular weight of ethanol?", "llama", "translate to SPARQL: What is the molecular weight of ethanol?\n\n###\n\n")
            ]
    )
    def test_preprocessInput(self, qn, model_family, expected):
        assert preprocess_input(qn, model_family=model_family) == expected