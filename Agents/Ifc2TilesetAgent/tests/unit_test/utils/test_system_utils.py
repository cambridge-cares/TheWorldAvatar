"""
# Author: qhouyee #

A test suite for the agent.utils.system_utils submodule.
"""

# Standard library imports
import os

# Third party import
import pytest

# Self import
from agent.utils import retrieve_abs_filepath, find_ifc_file
from agent.exceptions import InvalidInputError


@pytest.mark.parametrize(
    "relpath, expected",
    [
        (".", os.getcwd()),
        ("./agent", os.path.join(os.getcwd(), "agent"))
    ]
)
def test_retrieve_abs_filepath(relpath, expected):
    # Act
    actual = retrieve_abs_filepath(relpath)

    # Assert
    assert actual == expected


def test_read_ifc_file():
    # Arrange
    # Create file path of new IFC file
    ifcfile = os.path.join("data", "ifc", "temp.ifc")
    open(ifcfile, "x", encoding="utf-8").close()

    expected_path = os.path.join(os.getcwd(), ifcfile)

    # Act
    result_path = find_ifc_file(["data", "ifc"])

    # Assert
    assert result_path == expected_path


def test_read_ifc_file_empty_dir_fails():
    """Tests that the read_ifc_file() will fail in an empty directory."""
    # Act & Assert
    with pytest.raises(FileNotFoundError) as exc_info:
        find_ifc_file(["data", "ifc"])
    assert exc_info.match(r"^No ifc file is available at the ./data/ifc folder$")


def test_read_ifc_file_multiple_ifc_fails():
    """Tests that the read_ifc_file() will fail if there are multiple IFC files."""
    # Arrange
    ifcfile = os.path.join("data", "ifc", "temp.ifc")
    ifcfile2 = os.path.join("data", "ifc", "temp2.ifc")
    open(ifcfile, "x", encoding="utf-8").close()
    open(ifcfile2, "x", encoding="utf-8").close()

    # Act & Assert
    with pytest.raises(InvalidInputError) as exc_info:
        find_ifc_file(["data", "ifc"])
    assert exc_info.match(
        r"^More than one IFC file is located at the ./data/ifc folder. Please place only ONE IFC file.$")
