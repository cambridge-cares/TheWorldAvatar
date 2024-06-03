import pytest

from data_generation.create_training_data_from_kg.paraphrase import (
    sanitize_bulletpoint,
    parse_openai_paraphrase_response_content,
)


class TestParaphrase:
    @pytest.mark.parametrize(
        "text, expected",
        [
            (
                "1. How do individuals utilize (3-amino-2-oxopropyl) dihydrogen phosphate?",
                "How do individuals utilize (3-amino-2-oxopropyl) dihydrogen phosphate?",
            ),
            (
                "10. What's the covalent unit count of chemical entities belonging to aminoacridines?",
                "What's the covalent unit count of chemical entities belonging to aminoacridines?",
            ),
            (
                "- What are 4-piperonyl-3-(3,4,5-trimethoxybenzyl)tetrahydrofuran-2-one's fundamental properties?",
                "What are 4-piperonyl-3-(3,4,5-trimethoxybenzyl)tetrahydrofuran-2-one's fundamental properties?",
            ),
            (
                "3) I would like a list of chemical entities whose monoisotopic mass does not fall within the 165 to 182.80 range.",
                "I would like a list of chemical entities whose monoisotopic mass does not fall within the 165 to 182.80 range."
            )
        ],
    )
    def test_sanitizeBulletpoint(self, text, expected):
        assert sanitize_bulletpoint(text) == expected

    @pytest.mark.parametrize(
        "text, expected",
        [
            (
                "1. How do individuals utilize (3-amino-2-oxopropyl) dihydrogen phosphate?\n2. In what manner do people employ (3-amino-2-oxopropyl) dihydrogen phosphate?",
                [
                    "How do individuals utilize (3-amino-2-oxopropyl) dihydrogen phosphate?",
                    "In what manner do people employ (3-amino-2-oxopropyl) dihydrogen phosphate?",
                ],
            ),
            (
                '1. "How many covalent units do chemical compounds in the aminoacridine category have?"\n2. "Could you tell me the covalent unit count of chemical entities within the aminoacridine group?"',
                [
                    "How many covalent units do chemical compounds in the aminoacridine category have?",
                    "Could you tell me the covalent unit count of chemical entities within the aminoacridine group?",
                ],
            ),
            (
                'Certainly, here are 10 paraphrased versions of the statement:\n\n1. "How many covalent units are found in chemical entities associated with aminoacridines?"\n2. "Could you tell me the covalent unit tally for chemical substances within the aminoacridine group?"',
                [
                    "How many covalent units are found in chemical entities associated with aminoacridines?",
                    "Could you tell me the covalent unit tally for chemical substances within the aminoacridine group?",
                ],
            ),
            (
                "Can you provide information on the optical rotation and vapor pressure of CC(C(=O)C1=CC(=CC=C1)Cl)NC(C)(C)C?\nI'm curious about the optical rotation and vapor pressure of CC(C(=O)C1=CC(=CC=C1)Cl)NC(C)(C)C. Are you able to share any information?",
                [
                    "Can you provide information on the optical rotation and vapor pressure of CC(C(=O)C1=CC(=CC=C1)Cl)NC(C)(C)C?",
                    "I'm curious about the optical rotation and vapor pressure of CC(C(=O)C1=CC(=CC=C1)Cl)NC(C)(C)C. Are you able to share any information?",
                ],
            ),
            (
                "1. Provide a list of species with a sublimation enthalpy exceeding 284, renowned for their hormone production.\n2. Compile a catalog including species that possess a hormone and exhibit sublimation enthalpy values higher than 284.\n\nNote: The original statement appears to have some inconsistencies or unusual combination of criteria, as sublimation enthalpy is a property related to a substance's phase transition from solid to gas, while hormones are chemical compounds produced by living organisms.",
                [
                    "Provide a list of species with a sublimation enthalpy exceeding 284, renowned for their hormone production.",
                    "Compile a catalog including species that possess a hormone and exhibit sublimation enthalpy values higher than 284."
                ]
            ),
        ],
    )
    def test_parseOpenaiParaphraseResponseContent(self, text, expected):
        assert parse_openai_paraphrase_response_content(text) == expected
