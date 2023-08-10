import pytest

from marie.data_processing.abstract_query_rep import AbstractQueryRep


class TestQueryParser:
    def test_fromString(self):
        query_string = (
            "SELECT DISTINCT ?MolecularWeightValue\n"
            "WHERE {\n"
            "    ?SpeciesIRI ?hasIdentifier ?species .\n"
            '    FILTER( ?species = "C22H29FO4")\n'
            "    ?SpeciesIRI os:hasMolecularWeight ?MolecularWeightValue .\n"
            "}"
        )
        expected = AbstractQueryRep(
            "SELECT DISTINCT ?MolecularWeightValue",
            [
                "?SpeciesIRI ?hasIdentifier ?species .",
                'FILTER( ?species = "C22H29FO4")',
                "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightValue .",
            ],
            [],
        )
        assert AbstractQueryRep.from_string(query_string) == expected
