import pytest

from marie.data_processing.query_processing import (
    decode_special_chars,
    encode_special_chars,
    remove_prefixes,
)


class TestQueryUtils:
    @pytest.mark.parametrize(
        "query, expected",
        [
            (
                "SELECT *\nWHERE { ?s ?p ?o }\n",
                "SELECT *\nWHERE  op_br  var_s var_p var_o  cl_br \n",
            ),
            (
                "SELECT *\nWHERE { ?s ?p ?o FILTER()}\n",
                "SELECT *\nWHERE  op_br  var_s var_p var_o FILTER() cl_br \n",
            ),
        ],
    )
    def test_encodeQuery(self, query, expected):
        assert encode_special_chars(query) == expected

    @pytest.mark.parametrize(
            "query, expected",
            [
                ("SELECT *\nWHERE op_br var_s var_p var_o cl_br\n", "SELECT *\nWHERE { ?s ?p ?o }\n"),
                ("SELECT *\nWHERE op_br var_s var_p var_o FILTER()cl_br\n", "SELECT *\nWHERE { ?s ?p ?o FILTER()}\n")
            ]
    )
    def test_decodeQuery(self, query, expected):
        assert decode_special_chars(query) == expected

    @pytest.mark.parametrize(
        "query, expected",
        [
            (
                (
                    "PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>\n"
                    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
                    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
                    "SELECT *\nWHERE {?s ?p ?o}\n"
                ),
                "SELECT *\nWHERE {?s ?p ?o}\n",
            ),
            (
                (
                    "PREFIX os: \n<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>\n"
                    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
                    "PREFIX \nrdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
                    "SELECT *\nWHERE {?s ?p ?o}\n"
                ),
                "SELECT *\nWHERE {?s ?p ?o}\n",
            ),
            (
                "SELECT *\nWHERE {?s ?p ?o}\n",
                "SELECT *\nWHERE {?s ?p ?o}\n",
            ),
        ],
    )
    def test_removePrefixes(self, query, expected):
        assert remove_prefixes(query) == expected

    def test_preprocessQuery(self):
        pass

    def test_postprocessQuery(self):
        pass
