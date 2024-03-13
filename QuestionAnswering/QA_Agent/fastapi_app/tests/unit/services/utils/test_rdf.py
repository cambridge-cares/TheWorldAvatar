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

    @pytest.mark.parametrize(
        "subj,tails,expected",
        [
            (
                "https://www.theworldavatar.com/kg/landplot/LandUseType_51f02bc4-799e-4150-bf56-e55042503de7",
                [
                    (
                        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                        "https://www.theworldavatar.com/kg/ontozoning/MixedUse",
                    ),
                    (
                        "http://www.w3.org/2000/01/rdf-schema#comment",
                        "These are areas used or intended to be used mainly for commercial, hotel, residential, sports & recreational and other compatible uses, or a combination of two or more such uses as a mixed development.",
                    ),
                    (
                        "http://www.w3.org/2000/01/rdf-schema#label",
                        "Business Park - White",
                    ),
                ],
                """LandUseType_51f02bc4-799e-4150-bf56-e55042503de7
type: MixedUse
comment: These are areas used or intended to be used mainly for commercial, hotel, residential, sports & recreational and other compatible uses, or a combination of two or more such uses as a mixed development.
label: Business Park - White""",
            )
        ],
    )
    def test_linearizeNode(self, subj, tails, expected):
        assert linearize_node(subj, tails) == expected
