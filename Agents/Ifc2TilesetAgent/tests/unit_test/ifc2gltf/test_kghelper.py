"""
# Author: qhouyee #

A test suite for the agent.ifc2gltf.kghelper submodule.
"""

# Third party import
import pandas as pd

# Self import
from agent.ifc2gltf.kghelper import classify_file_name


def test_create_query():
    """
    Tests create_query()
    """
    pass


def test_classify_file_name():
    """
    Tests classify_file_name()
    """
    # Generate sample data
    data = {
        "name": ["Water Meter", "Solar Panel", "Desk", "Chemistry Robot (Detectors)"]
    }
    sampledf = pd.DataFrame(data)
    # Execute method
    sampledf = classify_file_name(sampledf)
    # Test assertions
    assert sampledf["file"].iloc[0] == "asset1"
    assert sampledf["file"].iloc[1] == "solarpanel"
    assert sampledf["file"].iloc[2] == "furniture"
    assert sampledf["file"].iloc[3] == "asset2"
