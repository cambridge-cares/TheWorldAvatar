import random
from typing import List

import nltk
from nltk.corpus import wordnet as wn


STOPWORDS = nltk.corpus.stopwords.words("english")


def synonym_sub(text: str, max_ops: int = 1):
    """Randomly substitutes some words in the input text with their synonym.

    Performs random substitition of non-stopwords up to `max_ops` times.

    Args:
        - text: An input string.
        - max_ops: Maximum number of substititions.

    Returns:
        A string with some of its words substituted.
    """
    if not text or not text.strip():
        return text

    tokens = nltk.word_tokenize(text)
    tokens_with_pos = nltk.pos_tag(nltk.tokenize.word_tokenize(text))

    nonstopword_idxes = get_nonstopword_alphabetical_token_idxes(tokens)

    if not nonstopword_idxes:
        return text

    nonstopword_idxes = random.sample(nonstopword_idxes, k=len(nonstopword_idxes))

    subs_count = 0
    for idx in nonstopword_idxes:
        if subs_count >= max_ops:
            break

        synonyms = get_synonyms(*tokens_with_pos[idx])
        if not synonyms:
            continue

        tokens[idx] = random.choice(synonyms)
        subs_count += 1

    return join_tokens(tokens)


def random_swap(text: str, max_ops: int = 1):
    """Randomly swaps two words in the input text."""
    if not text or not text.strip():
        return text

    tokens = nltk.word_tokenize(text)

    for _ in range(max_ops):
        idx1, idx2 = random.sample(range(len(tokens)), 2)
        tokens[idx1], tokens[idx2] = tokens[idx2], tokens[idx1]

    return join_tokens(tokens)


def random_insert(text: str, max_ops: int = 1):
    """Randomly inserts a synonym of a word to the input text."""
    if not text or not text.strip():
        return text

    tokens = nltk.word_tokenize(text)
    tokens_with_pos = nltk.pos_tag(nltk.tokenize.word_tokenize(text))

    nonstopword_idxes = get_nonstopword_alphabetical_token_idxes(tokens)

    if not nonstopword_idxes:
        return text

    nonstopword_idxes = random.sample(nonstopword_idxes, k=len(nonstopword_idxes))

    new_words = []
    inserts_count = 0
    for idx in nonstopword_idxes:
        if inserts_count >= max_ops:
            break

        synonyms = get_synonyms(*tokens_with_pos[idx])
        if not synonyms:
            continue

        new_words.append(random.choice(synonyms))
        inserts_count += 1

    for new_word in new_words:
        idx = random.randint(0, len(tokens))
        tokens.insert(idx, new_word)

    return join_tokens(tokens)


def random_delete(text: str, max_ops: int = 1):
    if not text or not text.strip():
        return text

    tokens = nltk.word_tokenize(text)
    candidate_delete_idxes = [i for i, token in enumerate(tokens) if token.isalpha()]
    candidate_delete_idxes = random.sample(
        candidate_delete_idxes, len(candidate_delete_idxes)
    )

    deletes_num = 0
    for idx in candidate_delete_idxes:
        if deletes_num >= max_ops:
            break
        tokens[idx] = ""
        deletes_num += 1

    return join_tokens(tokens)


def get_nonstopword_alphabetical_token_idxes(tokens: List[str]):
    """Returns a list of indices corresponding to the tokens that are
    alphabetical and not stopwords"""
    return [
        i
        for i, token in enumerate(tokens)
        if token.isalpha() and (token.lower() not in STOPWORDS)
    ]


def get_synonyms(word: str, treebank_pos: str):
    wn_pos = treebank2wn_pos(treebank_pos)
    if wn_pos is None:
        return []

    synonyms = []
    for ss in wn.synsets(word, pos=wn_pos):
        synonyms.extend(ss.lemma_names())

    synonyms = [word.replace("_", " ").replace("-", " ") for word in synonyms]
    synonyms = set(synonyms)
    if word in synonyms:
        synonyms.remove(word)

    return list(synonyms)


def treebank2wn_pos(treebank_tag: str):
    if treebank_tag.startswith("J"):
        return wn.ADJ
    if treebank_tag.startswith("V"):
        return wn.VERB
    if treebank_tag.startswith("N"):
        return wn.NOUN
    if treebank_tag.startswith("R"):
        return wn.ADV
    return None


PUNCTUATIONS = list(",.!?")
BRACKETS = list("()[]{}")
PUNCTUATIONS_AND_BRACKETS = PUNCTUATIONS + BRACKETS

def join_tokens(tokens: List[str]):
    output_text = []
    for i, token in enumerate(tokens):
        if (
            i > 0
            and token
            and token not in PUNCTUATIONS_AND_BRACKETS
            and output_text[-1] not in BRACKETS
        ):
            output_text.append(" ")
        output_text.append(token)
    return "".join(output_text)
