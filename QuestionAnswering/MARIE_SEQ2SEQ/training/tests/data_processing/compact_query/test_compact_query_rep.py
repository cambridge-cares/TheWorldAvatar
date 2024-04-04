import pytest
from core.data_processing.compact_query.compact_query_rep import CompactQueryRep


class TestComapctQueryRep:
    @pytest.mark.parametrize(
        "text, expected",
        [
            (
                'SELECT ?UseValue WHERE { VALUES ( ?species ) { ( "C12H21N5O2S2" ) } ?SpeciesIRI ?hasIdentifier ?species. ?SpeciesIRI os:hasUse ?UseValue. }',
                """SELECT DISTINCT ?label ?UseValue
WHERE {
    VALUES ( ?species ) { ( "C12H21N5O2S2" ) }
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI rdfs:label ?UseValue .
}""",
            ),
            (
                'SELECT ?XLogP3Value WHERE { VALUES ( ?species ) { ( "C13H32N4" ) } ?SpeciesIRI ?hasIdentifier ?species. ?SpeciesIRI os:hasXLogP3 ?XLogP3Value. }',
                """SELECT DISTINCT ?label ?XLogP3Value ?XLogP3UnitValue ?XLogP3ReferenceStateValue ?XLogP3ReferenceStateUnitValue
WHERE {
    VALUES ( ?species ) { ( "C13H32N4" ) }
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    ?SpeciesIRI os:hasXLogP3 ?XLogP3IRI .
    ?XLogP3IRI os:value ?XLogP3Value ; os:unit ?XLogP3UnitIRI ; os:hasProvenance ?XLogP3ProvenanceIRI . 
    ?XLogP3UnitIRI rdfs:label ?XLogP3UnitValue .
    OPTIONAL {
        ?XLogP3IRI os:hasReferenceState ?XLogP3ReferenceStateIRI .
        ?XLogP3ReferenceStateIRI os:value ?XLogP3ReferenceStateValue ; os:unit ?XLogP3ReferenceStateUnitIRI .
        ?XLogP3ReferenceStateUnitIRI rdfs:label ?XLogP3ReferenceStateUnitValue .
    }
}""",
            ),
        ],
    )
    def test_toVerbose(self, text, expected):
        assert CompactQueryRep.from_string(text).to_verbose() == expected
