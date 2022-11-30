"""
# Author: qhouyee #

This module provides utility functions for running shell command
and file operations, or performing list and regex searching operations.
"""

# Standard library imports
import os
import re
import subprocess


def run_shellcommand(command, require_shell=False):
    """
    Runs commands in the shell
    All non-error messages are suppressed

    Argument:
    command - Command arguments in the form ["command","command2"] or "command command2"
    require_shell - Boolean indicating if shell is required. Must be True for npm operations
    """
    subprocess.check_call(command, shell=require_shell,
                          stdout=subprocess.DEVNULL)


def install_glbconverter():
    """
    Installs the glb converter tool "gltf-pipeline" from npm through package.json
    """
    print("Checking for gltf-pipeline package from npm...")
    run_shellcommand("npm install -g gltf-pipeline", True)
    print("gltf-pipeline package is available!")


def retrieve_abs_filepath(filepath):
    """
    Runs commands in the shell
    All non-error messages are suppressed

    Argument:
    command - Command arguments in the form ["command","command2"] or "command command2"
    require_shell - Boolean indicating if shell is required. Must be True for npm operations
    """
    return os.path.abspath(filepath)


def read_ifc_file():
    """
    Reads IFC file located at ./data/ifc directory into required file paths
    """
    ifcpath = os.path.abspath(os.path.join(
        os.path.dirname(__file__), os.pardir, 'data', 'ifc'))
    filelist = [file for file in os.listdir(ifcpath) if os.path.isfile(
        os.path.join(ifcpath, file)) and not file == ".gitignore"]

    ifc_input = ""
    if not filelist:
        raise FileNotFoundError(
            'No ifc file is available at the ./data/ifc folder')
    elif len(filelist) == 1:
        ifc_input = os.path.join(ifcpath, filelist[0])
    else:
        raise RuntimeError(
            'More than one IFC file is located at the ./data/ifc folder. ' +
            'Please place only ONE IFC file')
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
                if "./data/ifc" not in filepath or filepath.endswith('.ttl'):
                    os.remove(filepath)
            except OSError:
                print("Error while deleting file")


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


# Run when module is imported
cleandir()
install_glbconverter()
