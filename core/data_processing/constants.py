T5_PREFIX_NL2SPARQL = "translate to SPARQL: "
T5_PREFIX_DOMAINCLS = "classify query domain: "

T5_NL_ENCODINGS = {"<": "&lt;", "<=": "&le;", ">": "&gt;", ">=": "&ge;", "^": "&Hat;"}
T5_SPARQL_ENCODINGS = {
    **T5_NL_ENCODINGS,
    **{
        "?": "var_",
        "{": "&lcub;",
        "}": "&rcub;",
    },
}
T5_SPARQL_DECODINGS = {v: k for k, v in T5_SPARQL_ENCODINGS.items()}