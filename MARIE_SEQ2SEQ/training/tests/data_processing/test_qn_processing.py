from marie.data_processing.qn_processing import t5_preprocess_qn


class TestQnProcessing:
    def test_preprocessQn(self):
        qn = "What is the molecular weight of ethanol?"
        expected = "translate to SPARQL: What is the molecular weight of ethanol?"
        assert t5_preprocess_qn(qn) == expected