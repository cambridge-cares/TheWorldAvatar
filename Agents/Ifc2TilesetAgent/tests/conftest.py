"""
# Author: picas9dan #

A module that provides pytest fixtures for all tests.
"""

# Standard library imports
import os
import shutil

# Third-party imports
import pytest


@pytest.fixture(scope="function", autouse=True)
def cleanup_data_dir():
    """Cleans up data dir and keeps only empty subfolders glb and ifc."""
    yield

    data_dir = "./data"
    for filename in os.listdir(data_dir):
        filepath = os.path.join(data_dir, filename)
        if os.path.isfile(filepath) or os.path.islink(filepath):
            os.remove(filepath)
        elif os.path.isdir(filepath):
            if filename in ("glb", "ifc"):
                _empty_dir(filepath)
            else:
                shutil.rmtree(filepath)


def _empty_dir(d: str):
    for filename in os.listdir(d):
        filepath = os.path.join(d, filename)
        if os.path.isfile(filepath) or os.path.islink(filepath):
            os.remove(filepath)
        elif os.path.isdir(filepath):
            shutil.rmtree(filepath)
