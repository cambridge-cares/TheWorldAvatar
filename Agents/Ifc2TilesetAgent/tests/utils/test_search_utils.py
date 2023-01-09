"""
# Author: qhouyee #

A test suite for the agent.utils.search_utils submodule.
"""

# Self import
from agent.utils import find_dictindex, find_word


def test_find_dictindex():
    """
    Tests find_dictindex()
    """
    # Generate sample list containing duplicate keys and unique values
    sample_key = "testkey"
    sample_value = "testvalue"
    sample_list = [{sample_key: "value1"}]
    sample_list.append({sample_key: "value2"})
    sample_list.append({sample_key: sample_value})
    # Test that key and value index are correct
    index_result = find_dictindex(sample_list, sample_key, sample_value)
    assert index_result == 2


def test_find_word():
    """
    Tests find_word()
    """
    # Generate sample words and string
    sample_words = ["test", "monday", "tuesday", "wednesday"]
    sample_string1 = "starting a long test"
    sample_string2 = "this is a monday"
    sample_string3 = "wrong answer only"
    # Test method, which returns True if found
    assert find_word(sample_words, sample_string1)
    assert find_word(sample_words, sample_string2)
    # Incorrect answer should return False
    assert not find_word(sample_words, sample_string3)
