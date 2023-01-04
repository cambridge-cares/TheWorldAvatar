"""
# Author: qhouyee #

A test suite for the agent.utils submodule.
"""

# Standard library imports
import os

# Third party import
import pytest

# Self import
from agent.utils import retrieve_abs_filepath, read_ifc_file, find_dictindex, find_word


def test_retrieve_abs_filepath():
    """
    Tests both potential outputs of retrieve_abs_filepath()
    """
    expected_path = os.getcwd()
    # Execute method
    filepath = retrieve_abs_filepath(".")
    assert filepath == expected_path

    expected_path = os.path.join(os.getcwd(), "agent")
    # Execute method
    filepath = retrieve_abs_filepath("./agent")
    assert filepath == expected_path


def test_read_ifc_file():
    """
    Tests read_ifc_file()
    """
    # Create file path of new IFC file
    ifcfile = os.path.join("data", "temp.ifc")
    expected_path = os.path.join(os.getcwd(), ifcfile)
    # Create a new file and execute method
    open(ifcfile, "x", encoding="utf-8")
    result_path = read_ifc_file(["data"])
    # Remove file once code has run and before assertions
    os.remove(ifcfile)
    assert result_path == expected_path


def test_read_ifc_file_empty_dir_fails():
    """
    Tests that the read_ifc_file() will fail in an empty directory
    """
    # Raise error with Empty directory containing no IFC file
    with pytest.raises(FileNotFoundError) as exc_info:
        read_ifc_file(["data"])
    assert exc_info.match(
        r"^No ifc file is available at the ./data/ifc folder$")


def test_read_ifc_file_multiple_ifc_fails():
    """
    Tests that the read_ifc_file() will fail if there are multiple IFC files
    """
    # Create two empty IFC files for testing
    ifcfile = os.path.join("data", "temp.ifc")
    ifcfile2 = os.path.join("data", "temp2.ifc")
    open(ifcfile, "x", encoding="utf-8")
    open(ifcfile2, "x", encoding="utf-8")
    # Raises error for directory with more than 1 IFC file
    with pytest.raises(RuntimeError) as exc_info:
        read_ifc_file(["data"])

    # Remove these files before assertion
    os.remove(ifcfile)
    os.remove(ifcfile2)
    assert exc_info.match(
        r"^More than one IFC file is located at the ./data/ifc folder. Please place only ONE IFC file$")


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
