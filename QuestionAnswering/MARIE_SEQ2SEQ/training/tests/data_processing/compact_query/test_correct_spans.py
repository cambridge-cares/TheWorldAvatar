import pytest
from core.data_processing.compact_query.compact_query_rep import CompactQueryRep
from core.data_processing.compact_query.correct_spans import SpanCorrector


class TestCorrectSpans:
    span_corrector = SpanCorrector()

    @pytest.mark.parametrize(
        "nlq, query, expected",
        [
            (
                "Share information regarding the optical rotation of hydrogen atom.",
                """SELECT ?OpticalRotationValue
WHERE { 
    VALUES ( ?species ) { ( "hydrogen atome" ) } 
    ?SpeciesIRI ?hasIdentifier ?species. 
    ?SpeciesIRI os:hasOpticalRotation ?OpticalRotationValue. 
}""",
                """SELECT ?OpticalRotationValue
WHERE {
    VALUES ( ?species ) { ( "hydrogen atom" ) }
    ?SpeciesIRI ?hasIdentifier ?species.
    ?SpeciesIRI os:hasOpticalRotation ?OpticalRotationValue.
}""",
            ),
        ],
    )
    def test_correct(self, nlq, query, expected):
        query = CompactQueryRep.from_string(query)
        assert self.span_corrector.correct(query=query, nlq=nlq).to_string() == expected
