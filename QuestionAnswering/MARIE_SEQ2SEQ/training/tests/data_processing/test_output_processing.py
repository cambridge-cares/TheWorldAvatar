import pytest

from core.data_processing.output_processing import (
    preprocess_output,
    postprocess_output,
    remove_prefixes,
)


class TestOutputProcessing:
    @pytest.mark.parametrize(
        "query, expected",
        [
            (
                "SELECT *\nWHERE { ?s ?p ?o }\n",
                "SELECT *\nWHERE &lcub; var_s var_p var_o &rcub;\n",
            ),
            (
                "SELECT *\nWHERE { ?s ?p ?o FILTER() }\n",
                "SELECT *\nWHERE &lcub; var_s var_p var_o FILTER() &rcub;\n",
            ),
        ],
    )
    def test_encodeOutput(self, query, expected):
        assert preprocess_output(query, model_family="t5") == expected

    @pytest.mark.parametrize(
        "query, expected",
        [
            (
                "SELECT *\nWHERE &lcub; var_s var_p var_o &rcub;\n",
                "SELECT *\nWHERE { ?s ?p ?o }\n",
            ),
            (
                "SELECT *\nWHERE &lcub; var_s var_p var_o FILTER()&rcub;\n",
                "SELECT *\nWHERE { ?s ?p ?o FILTER()}\n",
            ),
        ],
    )
    def test_decodeOutput(self, query, expected):
        assert postprocess_output(query, model_family="t5") == expected

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
