##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 07 Dec 2020                      #
##########################################
import configparser

config = configparser.RawConfigParser()
config.read('./conf/KB.properties')

def getTBoxIRI():
    return config.get('IRISection', 'kb.tbox.iri')

def getABoxIRI():
    return config.get('IRISection', 'kb.abox.iri')

if __name__ == '__main__':
    print(getTBoxIRI())
    print(getABoxIRI())