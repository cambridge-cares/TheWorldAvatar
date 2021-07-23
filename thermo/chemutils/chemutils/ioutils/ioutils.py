import os

def readFile(path):
    with open (os.path.abspath(path), "r") as myfile:
        data=myfile.read()
    return data

def writeFile(path,data):
    with open (os.path.abspath(path), "w") as myfile:
        myfile.write(data)

def fileExists(path):
    return os.path.exists(os.path.abspath(path))

def verbosityBasedOutput(output, silent, outFile):
    if outFile is not None: writeFile(outFile, output)
    if not silent: print(output)

def removeBlankTrailingLines(fileContentStr):
    fileContentStr = fileContentStr.split('\n')
    while not fileContentStr[-1].strip() and len(fileContentStr)>0: del fileContentStr[-1]
    return '\n'.join(fileContentStr)