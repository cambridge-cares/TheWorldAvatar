"""
# Author: qhouyee #

A test suite for the agent.utils.system_utils submodule.
"""

# Standard library imports
import os

# Third party import
import pytest

# Self import
from agent.utils import retrieve_abs_filepath, read_ifc_file
from agent.exceptions import InvalidInputError


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
    ifcfile = os.path.join("data", "ifc", "temp.ifc")
    expected_path = os.path.join(os.getcwd(), ifcfile)
    # Create a new file and execute method
    open(ifcfile, "x", encoding="utf-8")
    try:
        result_path = read_ifc_file(["data", "ifc"])
        assert result_path == expected_path
    finally:
        # Remove file once code has run and before assertions
        os.remove(ifcfile)


def test_read_ifc_file_empty_dir_fails():
    """
    Tests that the read_ifc_file() will fail in an empty directory
    """
    # Raise error with Empty directory containing no IFC file
    with pytest.raises(FileNotFoundError) as exc_info:
        read_ifc_file(["data", "ifc"])
    assert exc_info.match(
        r"^No ifc file is available at the ./data/ifc folder$")


def test_read_ifc_file_multiple_ifc_fails():
    """
    Tests that the read_ifc_file() will fail if there are multiple IFC files
    """
    # Create two empty IFC files for testing
    ifcfile = os.path.join("data", "ifc", "temp.ifc")
    ifcfile2 = os.path.join("data", "ifc", "temp2.ifc")
    open(ifcfile, "x", encoding="utf-8")
    open(ifcfile2, "x", encoding="utf-8")
    try:
        # Raises error for directory with more than 1 IFC file
        with pytest.raises(InvalidInputError) as exc_info:
            read_ifc_file(["data", "ifc"])
        assert exc_info.match(
            r"^More than one IFC file is located at the ./data/ifc folder. Please place only ONE IFC file$")
    finally:
        os.remove(ifcfile)
        os.remove(ifcfile2)
