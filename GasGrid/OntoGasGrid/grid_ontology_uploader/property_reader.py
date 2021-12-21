##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 21 Dec 2021                      #
##########################################
import configparser

config = configparser.RawConfigParser()
config.read('./conf/KG.properties')

def getSPARQLUpdateEndpoint():
    return config.get('SPARQLEndpoint', 'sparql.update.endpoint')

if __name__ == '__main__':
    print(getSPARQLUpdateEndpoint())
