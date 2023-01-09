"""
# Author: qhouyee #

This module provides utility functions for running shell command
and file operations, or performing list and regex searching operations.
"""

# Standard library imports
import os
import re
import subprocess

# Third party imports
from py4jps import agentlogging

# Retrieve logger
logger = agentlogging.get_logger("dev")

def run_shellcommand(command, require_shell=False):
    """
    Runs commands in the shell
    All non-error messages are suppressed

    Argument:
    command - Command arguments in the form ["command","command2"] or "command command2"
    require_shell - Boolean indicating if shell is required. Must be True for npm operations
    """
    subprocess.run(command, shell=require_shell)


def retrieve_abs_filepath(filepath):
    """
    Retrieves absolute file path from a relative file path

    Argument:
    filepath - A relative file path
    Returns:
    Absolute file path
    """
    if filepath == ".":
        return os.getcwd()
    else:
        return os.path.abspath(filepath)


def read_ifc_file(ifc_dir):
    """
    Reads IFC file located at ./data/ifc directory into required file paths

    Argument:
    ifc_dir - A list containing the nested directories to the IFC input from the root directory
            Eg:[data, ifc]
    Returns:
    File path to the IFC file
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
    else:
        errormsg = 'More than one IFC file is located at the ./data/ifc folder. '
        errormsg += 'Please place only ONE IFC file'
        logger.error(errormsg)
        raise RuntimeError(errormsg)
    return ifc_input


def cleandir():
    """
    Remove previously generated files from the directory while keeping any input ifc models
    """
    # Get a list of all files in directory
    for root_dir, subdirs, filelist in os.walk('./data/'):
        for filename in filelist:
            try:
                filepath = os.path.join(root_dir, filename)
                if  not (filepath.startswith("./data/ifc") or filepath.endswith('.gitignore')):
                    os.remove(filepath)
            except OSError:
                logger.error("Error while deleting file")


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
