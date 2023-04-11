"""
# Author: qhouyee, picas9dan #

A test suite for the agent.ifc2gltf.kghelper submodule.
"""

# Third-party imports
import pandas as pd

# Self imports
from agent.ifc2gltf.kghelper import classify_filename


def test_create_query():
    """
    Tests create_query()
    """
    pass


def test_classify_filename():
    # Arrange
    filenames = pd.Series(["Water Meter", "Solar Panel", "Desk", "Chemistry Robot (Detectors)"])
    expected = pd.Series(["asset1", "solarpanel", "furniture", "asset2"])

    # Act
    actual = classify_filename(filenames)

    # Assert
    assert (actual == expected).all()
