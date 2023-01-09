"""
# Author: qhouyee #

This module provides utility functions for searching in list, string, and regex.
"""

# Standard library imports
import re


def find_dictindex(lst, key, value):
    """
    Find the list index containing a specific key value pair
    """
    for i, dic in enumerate(lst):
        if dic[key] == value:
            return i
    return None


def find_word(wordlist, string):
    """
    Check if a word from a list exists in a string. Return true if found, and false otherwise
    """
    for word in wordlist:
        # Search for the word in the string and store the boolean result
        word_found = re.compile(r'\b({0})\b'.format(
            word), flags=re.IGNORECASE).search(string)
        if word_found:
            return True
    return False
