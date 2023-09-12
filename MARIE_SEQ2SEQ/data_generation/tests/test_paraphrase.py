import pytest

from data_generation.paraphrase import (
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
        ],
    )
    def test_sanitizeBulletpoint(self, text, expected):
        assert sanitize_bulletpoint(text) == expected

    @pytest.mark.parametrize(
        "text, expected",
        [
            (
                "1. How do individuals utilize (3-amino-2-oxopropyl) dihydrogen phosphate?\n2. In what manner do people employ (3-amino-2-oxopropyl) dihydrogen phosphate?\n3. three\n4. four\n5. five\n6. six\n7. seven\n8. eight\n9. nine\n10. ten",
                [
                    "How do individuals utilize (3-amino-2-oxopropyl) dihydrogen phosphate?",
                    "In what manner do people employ (3-amino-2-oxopropyl) dihydrogen phosphate?",
                    "three",
                    "four",
                    "five",
                    "six",
                    "seven",
                    "eight",
                    "nine",
                    "ten",
                ],
            ),
            (
                '1. "How many covalent units do chemical compounds in the aminoacridine category have?"\n2. "Could you tell me the covalent unit count of chemical entities within the aminoacridine group?"\n3. "three"\n4. "four"\n5. "five"\n6. "six"\n7. "seven"\n8. "eight"\n9. "nine"\n10. "ten"',
                [
                    "How many covalent units do chemical compounds in the aminoacridine category have?",
                    "Could you tell me the covalent unit count of chemical entities within the aminoacridine group?",
                    "three",
                    "four",
                    "five",
                    "six",
                    "seven",
                    "eight",
                    "nine",
                    "ten",
                ],
            ),
            (
                'Certainly, here are 10 paraphrased versions of the statement:\n\n1. "How many covalent units are found in chemical entities associated with aminoacridines?"\n2. "Could you tell me the covalent unit tally for chemical substances within the aminoacridine group?"\n3. "three"\n4. "four"\n5. "five"\n6. "six"\n7. "seven"\n8. "eight"\n9. "nine"\n10. "ten"',
                [
                    "How many covalent units are found in chemical entities associated with aminoacridines?",
                    "Could you tell me the covalent unit tally for chemical substances within the aminoacridine group?",
                    "three",
                    "four",
                    "five",
                    "six",
                    "seven",
                    "eight",
                    "nine",
                    "ten",
                ],
            ),
            (
                "Can you provide information on the optical rotation and vapor pressure of CC(C(=O)C1=CC(=CC=C1)Cl)NC(C)(C)C?\nI'm curious about the optical rotation and vapor pressure of CC(C(=O)C1=CC(=CC=C1)Cl)NC(C)(C)C. Are you able to share any information?\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten",
                [
                    "Can you provide information on the optical rotation and vapor pressure of CC(C(=O)C1=CC(=CC=C1)Cl)NC(C)(C)C?",
                    "I'm curious about the optical rotation and vapor pressure of CC(C(=O)C1=CC(=CC=C1)Cl)NC(C)(C)C. Are you able to share any information?",
                    "three",
                    "four",
                    "five",
                    "six",
                    "seven",
                    "eight",
                    "nine",
                    "ten",
                ],
            ),
        ],
    )
    def test_parseOpenaiParaphraseResponseContent(self, text, expected):
        assert parse_openai_paraphrase_response_content(text) == expected
