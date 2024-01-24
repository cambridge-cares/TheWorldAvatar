import pytest

from nltk.corpus import wordnet as wn

from data_generation.word_augmenters import (
    get_nonstopword_alphabetical_token_idxes,
    get_synonyms,
    join_tokens,
    treebank2wn_pos,
)


class TestWordAugmenters:
    def test_getNonstopwordAlphabeticalTokenIdxes(self):
        tokens = [
            "Could",
            "you",
            "liSt",
            "the",
            "chemical",
            "classes",
            "that",
            "heptadeca-9,16-dien-4,6-diyne-3,8-diol",
            "belongs",
            "to",
            "?",
        ]
        expected = [0, 2, 4, 5, 8]
        assert get_nonstopword_alphabetical_token_idxes(tokens) == expected

    def test_getSynonyms(self):
        word = "chemistry"
        expected = ["interpersonal chemistry", "chemical science", "alchemy"]
        assert set(get_synonyms(word, treebank_pos="NN")) == set(expected)

    @pytest.mark.parametrize(
        "treebank_tag, expected",
        [
            ("CD", None),
            ("JJS", wn.ADJ),
            ("NNPS", wn.NOUN),
            ("RBS", wn.ADV),
            ("VBZ", wn.VERB),
        ],
    )
    def test_treebank2wnPos(self, treebank_tag, expected):
        assert treebank2wn_pos(treebank_tag) == expected

    @pytest.mark.parametrize(
        "tokens, expected",
        [
            (["Could", "you", ",", "list", "?"], "Could you, list?"),
            (["Could", "you", "", "list", "?"], "Could you list?"),
            (
                [
                    "3-",
                    "[",
                    "2,5-bis",
                    "(",
                    "oxidanylidene",
                    ")",
                    "imidazolidin-4-yl",
                    "]",
                    "propanoic",
                    "acid",
                ],
                "3-[2,5-bis(oxidanylidene)imidazolidin-4-yl]propanoic acid",
            ),
        ],
    )
    def test_joinTokens(self, tokens, expected):
        assert join_tokens(tokens) == expected
