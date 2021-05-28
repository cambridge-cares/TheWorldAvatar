##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 07 Dec 2020                      #
##########################################
import configparser

config = configparser.RawConfigParser()
config.read('./conf/EntityRDFizer.properties')

"""Created variables to be used globally to maintain the values
read from a property file and to update the variables if users set
new values via setter functions"""
tboxIRI=''
aboxIRI=''
aboxFileName=''
aboxFileExtension=''
instanceLabelCreationOption=''

def getTBoxIRI():
    return tboxIRI

def setTBoxIRI(iri):
    global tboxIRI
    tboxIRI = iri

def getABoxIRI():
    return aboxIRI

def setABoxIRI(iri):
    global aboxIRI
    aboxIRI=iri

def getABoxFileName():
    return aboxFileName

def setABoxFileName(fileName):
    global aboxFileName
    aboxFileName = fileName

def getInstanceLabelCreationOption():
    return instanceLabelCreationOption

def setInstanceLabelCreationOption(labelCreationOption):
    global instanceLabelCreationOption
    instanceLabelCreationOption = labelCreationOption

def readInstanceLabelCreationOption():
    global instanceLabelCreationOption
    instanceLabelCreationOption = config.get('InstanceParameter', 'create.label.using.instance.name')
    return instanceLabelCreationOption

def getABoxFileExtension():
    return aboxFileExtension

def setABoxFileExtension(fileExtension):
    global aboxFileExtension
    aboxFileExtension = fileExtension

def readABoxFileExtension():
    global aboxFileExtension
    aboxFileExtension = config.get('FileSection', 'kb.abox.file.extension')
    return aboxFileExtension


if __name__ == '__main__':
    """Shows the default values available in the property file"""
    print(readInstanceLabelCreationOption())
    print(readABoxFileExtension())
    """Sets new values to update the ones read from the property file"""
    setTBoxIRI("http://a/test/tbox/iri")
    setABoxIRI("http://a/test/abox/iri")
    setABoxFileName("a-test-a-box-file-name")
    setInstanceLabelCreationOption("no")
    setABoxFileExtension("a-test-a-box-file-extension")
    """Shows the new values set via setter functions above"""
    print(getTBoxIRI())
    print(getABoxIRI())
    print(getABoxFileName())
    print(getInstanceLabelCreationOption())
    print(getABoxFileExtension())
