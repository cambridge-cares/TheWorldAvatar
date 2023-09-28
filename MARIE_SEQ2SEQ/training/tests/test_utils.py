import pytest

from core.utils import (
    advance_ptr_thru_space,
    advance_ptr_to_kw,
    advance_ptr_to_space,
    remove_terminal_chars,
)


class TestStrUtils:
    @pytest.mark.parametrize(
        "text, kw, idx, expected",
        [
            ("SELECT ?s WHERE {?s ?p ?o}", "?s", 0, 7),
            ("SELECT ?s WHERE {?s ?p ?o}", "?s", 11, 17),
            ("SELECT ?s WHERE {?s ?p ?o}", "FILTER", 0, 26),
        ],
    )
    def test_advanceIdxToBeforeKw(self, text, kw, idx, expected):
        assert advance_ptr_to_kw(text, kw, idx) == expected

    @pytest.mark.parametrize(
        "text, idx, expected",
        [
            ("SELECT ?s WHERE {}", 0, 0),
            ("SELECT \n\t?s WHERE {}", 6, 9),
        ],
    )
    def test_advanceIdxThruSpace(self, text, idx, expected):
        assert advance_ptr_thru_space(text, idx) == expected

    @pytest.mark.parametrize(
        "text, idx, expected",
        [
            ("SELECT ?s WHERE {}", 0, 6),
            ("SELECT\n \t?s WHERE {}", 3, 6),
            ("SELECT ?label", 6, 6)
        ],
    )
    def test_advanceIdxToSpace(self, text, idx, expected):
        assert advance_ptr_to_space(text, idx) == expected

    @pytest.mark.parametrize(
        "text, expected",
        [
            ("What is the charge of benzene?", "What is the charge of benzene"),
            (
                "What is the boiling point of water??",
                "What is the boiling point of water",
            ),
            (
                "Tell me the heavy atom count in ethanol.",
                "Tell me the heavy atom count in ethanol",
            ),
            ("air pressure", "air pressure"),
        ],
    )
    def test_removeTerminalPunctuations(self, text, expected):
        assert remove_terminal_chars(text) == expected
