##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 21 Dec 2021                      #
##########################################
import configparser

config = configparser.RawConfigParser()
config.read('./conf/uploader.properties')

def getTripleStoreURL():
    return config.get('TripleStore', 'triple.store.url')

def getTripleStoreNamespace():
    return config.get('TripleStore', 'triple.store.namespace')

def getGridComponentABoxFilePath():
    return config.get('OntologyFilePath', 'grid.component.file.path')

def getPipelineABoxFilePath():
    return config.get('OntologyFilePath', 'pipeline.file.path')

if __name__ == '__main__':
    print(getTripleStoreURL())
    print(getTripleStoreNamespace())
    print(getGridComponentABoxFilePath())
    print(getPipelineABoxFilePath())
