##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 07 Dec 2020                      #
##########################################
import configparser

config = configparser.RawConfigParser()
config.read('./conf/EntityRDFizer.properties')

tboxIRI=''
aboxIRI=''
aboxFileName=''
aboxFileExtension=''

def getTBoxIRI():
    return config.get('IRISection', 'kb.tbox.iri')

def getABoxIRI():
    return config.get('IRISection', 'kb.abox.iri')

def getABoxFileName():
    return config.get('FileSection', 'kb.abox.file.name')

def getABoxFileExtension():
    return config.get('FileSection', 'kb.abox.file.extension')

def setTBoxIRI(tboxIRI):
    global.tboxIRI=tboxIRI

def setABoxIRI(aboxIRI):
    global.aboxIRI=aboxIRI

def setABoxFileName(aboxFileName):
    global.aboxFileName=aboxFileNAme

def setABoxFileExtension():
    return config.get('FileSection', 'kb.abox.file.extension')

if __name__ == '__main__':
    print(getTBoxIRI())
    print(getABoxIRI())
    print(getABoxFileName())
    print(getABoxFileExtension())

