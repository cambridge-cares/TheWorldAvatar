"""
# Author: qhouyee #

This module is a python script to download the required resources.
"""

# Standard library imports
from io import BytesIO
import shutil
import zipfile
import sys

# Third party imports
import requests


def download(url):
    """
    Get the download request from the URL and return the object and file name
    """
    # Downloading the file by sending the request to the URL
    req = requests.get(url)
    # Split URL to get the file name
    filename = url.split('/')[-1]
    return req, filename


def dl_zip(url, outpath_enum=1, extractall=True):
    """
    Download and extract a zip file into the required file path
    Used to download COLLADA2GLTF and IFC Convert resources and ifcopenshell python packages

    Arguments:
     url - download url to retrieve zip file
     outpath_enum - an enum to determine output path based on the resource
     extractall - a boolean to establish if we are interested in all the contents
                  or a specific content in the zip file

    """
    req, filename = download(url)

    # extracting the zip file contents
    zip_output = zipfile.ZipFile(BytesIO(req.content))

    # Match enum input to the required paths
    if outpath_enum == 1:
        output_path = "./agent/resources/"
    elif outpath_enum == 2:
        # Edit the environment name
        output_path = "bimv/Lib/site-packages/"
        if "<venv>" in output_path:
            print(
                "Please change <venv> in line 58 to your python virtual environment name")
            exit()
    elif outpath_enum < 1 or outpath_enum > 2:
        print("valid input for outpath_enum: 1 and 2")

    # If we want to extract all content from the zip file
    if extractall:
        zip_output.extractall(output_path)
    # If we want to extract specific content from the zip file
    # For the required Python packages
    else:
        for zip_info in zip_output.infolist():
            if "ifcopenshell" in zip_info.filename:
                zip_output.extract(zip_info.filename, ".")
        shutil.move("blenderbim/libs/site/packages/ifcopenshell", output_path)
        shutil.rmtree("blenderbim")

    print("Downloading " + filename + " Completed")


def downloadtask():
    """
    Downloads the required resources according to python version in the Windows operating system
    """
    # Download and extract zip files
    dl_zip("https://s3.amazonaws.com/ifcopenshell-builds/IfcConvert-v0.7.0-cdde536-win64.zip")

    # Download and extract python packages according to python version
    if sys.version_info >= (3, 7, 0) and sys.version_info < (3, 9, 0):
        dl_zip("https://github.com/IfcOpenShell/IfcOpenShell/releases/download/blenderbim-221121/blenderbim-221121-py37-win.zip", 2, False)
    elif sys.version_info >= (3, 9, 0) and sys.version_info < (3, 10, 0):
        dl_zip("https://github.com/IfcOpenShell/IfcOpenShell/releases/download/blenderbim-221121/blenderbim-221121-py39-win.zip", 2, False)
    elif sys.version_info >= (3, 10, 0):
        dl_zip("https://github.com/IfcOpenShell/IfcOpenShell/releases/download/blenderbim-221121/blenderbim-221121-py310-win.zip", 2, False)
    else:
        print("Python version is outdated. Please update python version to at least 3.7.0 or above")


downloadtask()
