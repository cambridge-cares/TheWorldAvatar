import pytest
from core.data_processing.compact_query.compact_query_rep import CompactQueryRep
from core.data_processing.compact_query.correct_relations import RelationCorrector, tokenize


class TestCorrectRelations:
    relation_corrector = RelationCorrector()
    
    @pytest.mark.parametrize(
        "text, tokens",
        [
            (
                "StandardEnthalpyOfFormation",
                ["Standard", "Enthalpy", "Of", "Formation"],
            ),
            ("CanonicalizedCompound", ["Canonicalized", "Compound"]),
            ("Caco2Permeability", ["Caco2", "Permeability"]),
            ("XLogP3", ["XLogP3"]),
            ("LogS", ["Log", "S"]),
            ("ChebiID", ["Chebi", "ID"]),
            ("CID", ["CID"]),
            ("IUPACName", ["IUPAC", "Name"]),
            ("InChIKey", ["InChIKey"]),
            ("SMILES", ["SMILES"]),
        ],
    )
    def test_tokenize(self, text, tokens):
        assert tokenize(text) == tokens

    @pytest.mark.parametrize(
        "query, expected",
        [
            (
                """SELECT ?OpticalRotationValue
WHERE {
    VALUES ( ?species ) { ( "hydrogen atom" ) }
    ?SpeciesIRI ?hasIdentifier ?species .
    ?SpeciesIRI os:hasOpticRotation ?OpticalRotationValue .
}""",
                """SELECT ?OpticalRotationValue
WHERE {
    VALUES ( ?species ) { ( "hydrogen atom" ) }
    ?SpeciesIRI ?hasIdentifier ?species .
    ?SpeciesIRI os:hasOpticalRotation ?OpticalRotationValue .
}""",
            ),
        ],
    )
    def test_correct(self, query, expected):
        query = CompactQueryRep.from_string(query)
        assert self.relation_corrector.correct(query).to_string() == expected
