import pytest

from marie.services.preprocessing import (
    advance_past_magnitude,
    advance_past_unit,
    advance_to_magnitude,
    is_magnitude_char,
    sanitize_quantities,
    startswith_magnitude,
)


class TestPreprocessing:
    @pytest.mark.parametrize(
        "text, expected",
        [
            ("5", True),
            ("5 g", True),
            ("-40 °F", True),
            ("20.80 mmHg", True),
            (".3 ppm", True),
            (" 1 K", False),
            ("one F", False),
        ],
    )
    def test_startswithMagnitude(self, text, expected):
        assert startswith_magnitude(text) == expected

    @pytest.mark.parametrize(
        "char, expected",
        [
            ("-", True),
            (".", True),
            ("*", True),
            ("+", True),
            ("e", True),
            ("E", True),
            ("x", True),
            ("4", True),
            ("b", False),
        ],
    )
    def test_isMagnitudeChar(self, char, expected):
        assert is_magnitude_char(char) == expected

    @pytest.mark.parametrize(
        "text, ptr, expected",
        [
            ("5 g/ml", 0, 1),
            ("8.20x10**+1 ppm and 170°C", 0, 11),
            ("8.20x10**+1 ppm and 170°C", 12, 12),
            ("8.20x10**+1 ppm and 170°C", 20, 23),
            ("8.20x10**+1 ppm and 170°C", 23, 23),
        ],
    )
    def test_advancePastMagnitude(self, text, ptr, expected):
        assert advance_past_magnitude(text, ptr) == expected

    @pytest.mark.parametrize(
        "text, ptr, expected",
        [
            ("5 g/ml", 1, 6),
            ("8.20x10**+1 ppm and 170°C", 12, 15),
        ],
    )
    def test_advancePastUnit(self, text, ptr, expected):
        assert advance_past_unit(text, ptr) == expected

    @pytest.mark.parametrize(
        "text, ptr, expected",
        [
            ("5 g/ml", 0, 0),
            ("8.20x10**+1 ppm and 170°C", 12, 20),
            ("8.20x10**+1 ppm and 170°C", 23, 25),
            ("What is the exact mass of C16H34O?", 0, 34)
        ],
    )
    def test_advanceToMagnitude(self, text, ptr, expected):
        assert advance_to_magnitude(text, ptr) == expected

    @pytest.mark.parametrize(
        "text, expected",
        [
            (
                "Catalog compounds wherein the logS value is lower than 29",
                dict(
                    preprocessed_text_for_user="Catalog compounds wherein the logS value is lower than 29",
                    preprocessed_text_for_trans="Catalog compounds wherein the logS value is lower than 29",
                ),
            ),
            (
                "Find all species with boiling point above 0°C",
                dict(
                    preprocessed_text_for_user="Find all species with boiling point above 273.15 K",
                    preprocessed_text_for_trans="Find all species with boiling point above 273.15",
                ),
            ),
            (
                "What are the chemical species having molecular weight between 150 g/mol and 1 kg/mol?",
                dict(
                    preprocessed_text_for_user="What are the chemical species having molecular weight between 150.0 g / mol and 1000.0 g / mol?",
                    preprocessed_text_for_trans="What are the chemical species having molecular weight between 150.0 and 1000.0?",
                ),
            ),
            (
                "Enumerate molecules with solubility exceeding 480 g/L and falling into the aromatic amine classification",
                dict(
                    preprocessed_text_for_user="Enumerate molecules with solubility exceeding 479.99999999999994 kg / m ** 3 and falling into the aromatic amine classification",
                    preprocessed_text_for_trans="Enumerate molecules with solubility exceeding 479.99999999999994 and falling into the aromatic amine classification",
                )
            )
        ],
    )
    def test_sanitizeQuantities(self, text, expected):
        assert sanitize_quantities(text) == expected
