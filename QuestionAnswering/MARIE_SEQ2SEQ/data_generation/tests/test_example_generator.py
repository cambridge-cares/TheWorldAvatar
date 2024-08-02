import pytest

from data_generation.example_generator import get_argnames


class TestExampleGenerator:
    @pytest.mark.parametrize(
        "text, argnames",
        [
            (
                "Give me details about the {PropertyName} of molecules classed as {ChemClass}.",
                ["PropertyName", "ChemClass"],
            )
        ],
    )
    def test_getArgnames(self, text, argnames):
        assert set(get_argnames(text)) == set(argnames)
