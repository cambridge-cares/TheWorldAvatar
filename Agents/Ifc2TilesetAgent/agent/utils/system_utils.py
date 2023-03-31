"""
# Author: qhouyee #

This module provides utility functions for running operations on system,
such as shell command and file operations.
"""

# Standard library imports
import os
import subprocess
from typing import List, Union

# Third party imports
from py4jps import agentlogging

# Self imports
from agent.exceptions import InvalidInputError

# Retrieve logger
logger = agentlogging.get_logger("dev")


def run_shellcommand(command: Union[str, List[str]], require_shell: bool = False):
    """Runs commands in the shell.

    All non-error messages are suppressed.

    Args:
        command: Command arguments in the form ["command","command2"] or "command command2".
        require_shell: A bool indicating if shell is required. Must be True for npm operations.
    """
    subprocess.run(command, shell=require_shell)


def retrieve_abs_filepath(filepath: str):
    """Retrieves the absolute filepath from a relative filepath. """
    if filepath == ".":
        return os.getcwd()
    else:
        return os.path.abspath(filepath)


def find_ifc_file(ifc_dir: List[str]):
    """Retrieves the path to the IFC file located at ./data/ifc directory.

    Args:
        ifc_dir: A list containing the nested directories to the IFC input from the root directory.
                 E.g. [data, ifc]
    Returns:
        File path to the IFC file.

    Raises:
        FileNotFoundError: No IFC file is found in the provided directory.
        InvalidInputError: More than one IFC files are found in the provided directory.
    """
    # If empty list,
    if not ifc_dir:
        ifc_relpath = "."
    else:
        for directory in ifc_dir:
            # Check if this variable exist
            try:
                ifc_relpath
            # If it doesn't exist, initialise the variable
            except NameError:
                ifc_relpath = os.path.join(".", directory)
            # If it exist, append to filepath
            else:
                ifc_relpath = os.path.join(ifc_relpath, directory)

    ifcpath = retrieve_abs_filepath(ifc_relpath)

    filelist = [file for file in os.listdir(ifcpath)
                if os.path.isfile(os.path.join(ifcpath, file)) and not file == ".gitignore"]
    ifc_input = ""
    if not filelist:
        errormsg = 'No ifc file is available at the ./data/ifc folder'
        logger.error(errormsg)
        raise FileNotFoundError(errormsg)
    elif len(filelist) == 1:
        ifc_input = os.path.join(ifcpath, filelist[0])
        logger.debug("One IFC file detected: " + ifc_input)
    elif len(filelist) > 1:
        errormsg = 'More than one IFC file is located at the ./data/ifc folder. '
        errormsg += 'Please place only ONE IFC file'
        logger.error(errormsg)
        raise InvalidInputError(errormsg)
    return ifc_input


def cleandir():
    """Removes previously generated files from the directory while keeping any input IFC models."""
    # Get a list of all files in directory
    for root_dir, subdirs, filelist in os.walk('./data/'):
        for filename in filelist:
            try:
                filepath = os.path.join(root_dir, filename)
                if not (filepath.startswith("./data/ifc") or filepath.endswith('.gitignore')):
                    os.remove(filepath)
            except OSError:
                logger.error("Error while deleting file")
