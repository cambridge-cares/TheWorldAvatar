import pytest

from core.data_processing.output_processing import (
    t5_decode_output_special_chars,
    t5_encode_output_special_chars,
    normalize_query,
    remove_prefixes,
)


class TestOutputProcessing:
    @pytest.mark.parametrize(
        "query, expected",
        [
            (
                "SELECT *\nWHERE { ?s ?p ?o }\n",
                "SELECT *\nWHERE op_br var_s var_p var_o cl_br\n",
            ),
            (
                "SELECT *\nWHERE { ?s ?p ?o FILTER() }\n",
                "SELECT *\nWHERE op_br var_s var_p var_o FILTER() cl_br\n",
            ),
        ],
    )
    def test_encodeOutput(self, query, expected):
        assert t5_encode_output_special_chars(query) == expected

    @pytest.mark.parametrize(
        "query, expected",
        [
            (
                "SELECT *\nWHERE op_br var_s var_p var_o cl_br\n",
                "SELECT *\nWHERE { ?s ?p ?o }\n",
            ),
            (
                "SELECT *\nWHERE op_br var_s var_p var_o FILTER()cl_br\n",
                "SELECT *\nWHERE { ?s ?p ?o FILTER()}\n",
            ),
        ],
    )
    def test_decodeOutput(self, query, expected):
        assert t5_decode_output_special_chars(query) == expected

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

    @pytest.mark.parametrize(
        "query, expected",
        [
            (
                """SELECT DISTINCT ?label ?ChemicalClassValue 
WHERE {
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "InChI=1S/C6H8O3/c1-6(2)3-9-5(8)4(6)7/h3H2,1-2H3")

	?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .
}""",
                'SELECT DISTINCT ?label ?ChemicalClassValue WHERE { ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label . ?SpeciesIRI ?hasIdentifier ?IdentifierIRI . ?IdentifierIRI rdf:type ?Identifier ; os:value ?species . ?Identifier rdfs:subClassOf os:Identifier . FILTER ( ?species = "InChI=1S/C6H8O3/c1-6 ( 2 ) 3-9-5 ( 8 ) 4 ( 6 ) 7/h3H2 , 1-2H3" ) ?SpeciesIRI os:hasChemicalClass* ?x . ?x ?y ?z . ?z rdfs:subClassOf* ?ChemicalClassIRI . ?ChemicalClassIRI rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue . }',
            )
        ],
    )
    def test_normalizeQuery(self, query, expected):
        assert normalize_query(query) == expected
