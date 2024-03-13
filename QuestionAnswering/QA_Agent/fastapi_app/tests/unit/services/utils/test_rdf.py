import pytest
from services.utils.rdf import extract_name, linearize_node


class TestRdf:
    @pytest.mark.parametrize(
        "iri,expected",
        [
            (
                "https://www.theworldavatar.com/kg/ontoplanningregulation/CompetentAuthority",
                "CompetentAuthority",
            ),
            ("http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "type"),
        ],
    )
    def test_extractName(self, iri, expected):
        assert extract_name(iri) == expected
