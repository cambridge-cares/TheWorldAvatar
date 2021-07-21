import sys
import os
import json
import glob

def wait():
	input("Press Enter to continue...")

def codexit():
    wait()
    sys.exit()

def dienicely(errmsg):
    print(errmsg)
    codexit()

def readFile(path):
    with open (os.path.abspath(path), "r") as myfile:
        data=myfile.read()
    return data

def getRefName(logFile,jobIndex,numJobs,extension):
    if numJobs > 1:
        refName = logFile + '_' + str(jobIndex+1)+extension
    else:
        refName = logFile + extension
    return refName

def qc_log_to_json(parsedJobsList, outDir, outFileBaseName, extension='.json'):

    for jobIndex, jobDataJson in enumerate(parsedJobsList):
        outFileName = getRefName(outFileBaseName,jobIndex=jobIndex,numJobs=len(parsedJobsList), extension=extension)
        outFilePath = os.path.join(outDir, outFileName)

        dictData = json.loads(jobDataJson)
        with open(outFilePath, 'w') as outfile:
            json.dump(dictData, outfile, indent = 4)

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

def fileExists(path):
    return os.path.isfile(os.path.abspath(path))

def dirExists(path):
    return os.path.isdir(os.path.abspath(path))