import pytest

from core.data_processing.compact_query.utils import remove_terminal_chars


class TestCompactQueryRepUtils:
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
    def test_removeTerminalChars(self, text, expected):
        assert remove_terminal_chars(text) == expected
