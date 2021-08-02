import os
import glob

def readFile(path, *args, **kwargs):
    with open (os.path.abspath(path),'r', *args, **kwargs) as myfile:
        data=myfile.read()
    return data

def writeFile(path,data, *args, **kwargs):
    with open (os.path.abspath(path), 'w', *args, **kwargs) as myfile:
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
        files = [fileOrDir]
    elif dirExists(fileOrDir):
        for fileExt in fileExtList:
            files+=glob.glob(os.path.join(fileOrDir,'*'+fileExt))
    else:
        raise FileNotFoundError('Error: File or directory: "'+fileOrDir+'" does not exists.')
    return files
