"""
# Author: qhouyee #

This module provides utility functions for searching in list, string, and regex.
"""

# Standard library imports
import re
from typing import List


def find_word(wordlist: List[str], string: str):
    """Checks if a word from a list exists in a string.

    Args:
        wordlist: A list of words.
        string: A string to search against words in wordlist.

    Returns:
        True if any element in wordlist is found in string, False otherwise.
        The found occurrence must be separated with preceding or following non-whitespace characters by whitespaces.
    """
    for word in wordlist:
        # Search for the word in the string and store the boolean result
        word_found = re.compile(r'\b({0})\b'.format(word), flags=re.IGNORECASE).search(string)
        if word_found:
            return True
    return False
