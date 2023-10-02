import pytest
from core.data_processing.compact_query.correct_relations import tokenize


class TestCorrectRelations:
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
