##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 06 Jan 2021                      #
##########################################
import configparser

config = configparser.RawConfigParser()
config.read('./conf/GMLParser.properties')

def getNOfMapsInAnAboxFile():
    return config.get('ABOX', 'number.of.maps.in.an.abox.file')

if __name__ == '__main__':
    print(getNOfMapsInAnAboxFile())
