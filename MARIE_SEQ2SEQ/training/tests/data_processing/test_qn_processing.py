from core.data_processing.qn_processing import t5_preprocess_qn, llama_preprocess_qn


class TestQnProcessing:
    def test_t5PreprocessQn(self):
        qn = "What is the molecular weight of ethanol?"
        expected = "translate to SPARQL: What is the molecular weight of ethanol?"
        assert t5_preprocess_qn(qn) == expected

    def test_llamaPreprocessQn(self):
        qn = "What is the molecular weight of ethanol?"
        expected = "translate to SPARQL: What is the molecular weight of ethanol?\n\n###\n\n"
        assert llama_preprocess_qn(qn) == expected