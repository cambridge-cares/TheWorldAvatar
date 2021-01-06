##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 06 Jan 2021                      #
##########################################
import configparser

config = configparser.RawConfigParser()
config.read('./conf/GMLParser.properties')

def getNOfMapsInAnAboxFile():
    return config.get('ABOX', 'number.of.maps.in.an.abox.file')

def getClassLinearRing():
    return config.get('TBOX', 'class.linear.ring')

def getPropertyPosList():
    return config.get('TBOX', 'property.pos.list')

if __name__ == '__main__':
    print(getNOfMapsInAnAboxFile())
    print(getClassLinearRing())
    print(getPropertyPosList())
