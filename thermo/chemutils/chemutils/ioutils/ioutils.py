import os
import glob

def readFile(path, *args, **kwargs):
    with open (os.path.abspath(path), 'r', *args, **kwargs) as myfile:
        data=myfile.read()
    return data

def writeFile(path, data, *args, **kwargs):
    with open (os.path.abspath(path), 'w', *args, **kwargs) as myfile:
        myfile.write(data)

def fileExists(path):
    return os.path.isfile(os.path.abspath(path))

def dirExists(path):
    return os.path.isdir(os.path.abspath(path))

def getBaseDirPath(fileOrDir):
    if fileExists(fileOrDir):
        return os.path.dirname(fileOrDir)
    elif dirExists(fileOrDir):
        return fileOrDir
    else:
        raise FileNotFoundError('Error: File or directory does not exists')

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
    return files

def getFileBaseName(filePath):
    return os.path.splitext(filePath)[0]

def getFileNameWithExt(filePath):
    return os.path.split(filePath)