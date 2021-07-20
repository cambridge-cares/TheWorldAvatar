import os
import pathlib

def readFile(path):
    with open (os.path.abspath(path), "r") as myfile:
        data=myfile.read()
    return data

def writeFile(path,data):
    with open (os.path.abspath(path), "w") as myfile:
        myfile.write(data)

def fileExists(path):
    return os.path.isfile(os.path.abspath(path))

def dirExists(path):
    return os.path.isdir(os.path.abspath(path))

def verbosityBasedOutput(output, silent, outFile):
    if outFile is not None: writeFile(outFile, output)
    if not silent: print(output)

def removeBlankTrailingLines(fileContentStr):
    fileContentStr = fileContentStr.split('\n')
    while not fileContentStr[-1].strip() and len(fileContentStr)>0: del fileContentStr[-1]
    return '\n'.join(fileContentStr)

def getFilesWithExtensions(fileOrDir, fileExtList):
    files = []
    if fileExists(fileOrDir):
        fileExt = os.path.splitext(fileOrDir)[1]
        if fileExt in fileExtList: files = [fileOrDir]
    elif dirExists(fileOrDir):
        for fileExt in fileExtList:
            files.extend([p.resolve() for p in list(pathlib.Path(fileOrDir).glob('*'+fileExt))])
    else:
        raise FileNotFoundError('Error: File or directory: "'+fileOrDir+'" does not exists.')
    return files
