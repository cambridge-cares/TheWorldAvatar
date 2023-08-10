TRANS_MODEL_INPUT_PREFIX = "translate to SPARQL: "

def preprocess_qn(qn: str):
    qn = TRANS_MODEL_INPUT_PREFIX + qn
    # convert units
    return qn
