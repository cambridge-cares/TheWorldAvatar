# Standard library imports
from io import BytesIO
import shutil, zipfile, sys

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

def dl_file(url):
    """
    Download a single file into the required file path
    Used to download IFCtoRDF and Blazegraph jar resources
    """
    req, filename = download(url)
    
    # Writing the file to the local file system
    output_path="resources/"+filename
    with open(output_path,'wb') as output_file:
        output_file.write(req.content)

    print("Downloading " + filename+ " Completed")

def dl_zip(url,outpath_enum=1,extractall=True):
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
    zip= zipfile.ZipFile(BytesIO(req.content))

    # Match enum input to the required paths
    if outpath_enum==1:
        output_path = "./resources/"
    if outpath_enum==2:
        output_path = "./resources/COLLADA2GLTF"
    if outpath_enum==3:
        #Edit the environment name
        output_path = "<venv>/Lib/site-packages/"
        if "<venv>" in output_path:
            print("Please change <venv> in line 58 to your python virtual environment name")
            exit()
    if outpath_enum<1 or outpath_enum>3:
        print("valid input for outpath_enum: 1, 2, and 3")
    
    # If we want to extract all content from the zip file
    if extractall== True:
        zip.extractall(output_path)
    
    # If we want to extract specific content from the zip file
    # For the required Python packages
    else:
        for zip_info in zip.infolist():
            if "ifcpatch" or "ifcopenshell" in zip_info.filename:
                zip.extract(zip_info.filename,".")
        
        shutil.move("blenderbim/libs/site/packages/ifcpatch", output_path) 
        shutil.move("blenderbim/libs/site/packages/ifcopenshell", output_path)
        shutil.rmtree("blenderbim")

    print("Downloading " + filename+ " Completed")

def downloadtask():
    """
    Downloads the required resources according to python version in the Windows operating system
    """
    # Download and extract zip files
    dl_zip("https://s3.amazonaws.com/ifcopenshell-builds/IfcConvert-v0.7.0-cdde536-win64.zip")
    dl_zip("https://github.com/KhronosGroup/COLLADA2GLTF/releases/download/v2.1.5/COLLADA2GLTF-v2.1.5-windows-Release-x64.zip",2)

    # Download jar files
    dl_file("https://github.com/pipauwel/IFCtoRDF/releases/download/IFCtoRDF-0.4/IFCtoRDF-0.4-shaded.jar")
    dl_file("https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_RELEASE_2_1_5/blazegraph.jar")

    #Download and extract python packages according to python version
    if sys.version_info >= (3, 7, 0) and sys.version_info < (3, 9, 0):
        dl_zip("https://github.com/IfcOpenShell/IfcOpenShell/releases/download/blenderbim-221121/blenderbim-221121-py37-win.zip",3,False)
    elif sys.version_info >= (3, 9, 0) and sys.version_info < (3, 10, 0):
        dl_zip("https://github.com/IfcOpenShell/IfcOpenShell/releases/download/blenderbim-221121/blenderbim-221121-py39-win.zip",3,False)
    elif sys.version_info >= (3, 10, 0):        
        dl_zip("https://github.com/IfcOpenShell/IfcOpenShell/releases/download/blenderbim-221121/blenderbim-221121-py310-win.zip",3,False)
    else:
        print("Python version is outdated. Please update python version to at least 3.7.0 or above")

downloadtask()  