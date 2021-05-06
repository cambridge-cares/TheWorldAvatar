##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 19 April 2021        #
##########################################

"""This module defines the sub-graph memebers of UK digital twin and the URI fragments used to construct the A-box"""

# Level of the node is different of the level of the knowledge graph. The level of the KG is depends on the degree of data being processed, 
# i. e. the data in Level 3 KG is processed based on the raw data from Level 1 and 3 
# Level 1 KG is powerPlant, energyConsumption; Level 2 KG contains gridTopology; Level 3 KG includes powerGridModel.

class UKDigitalTwin:
    
    """Notation used in URI construction"""
    HASH = '#'
    SLASH = '/'
    UNDERSCORE = '_'
    OWL = '.owl'
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\Top_node\\"
    
    """ Node Names """
    baseNode = "http://www.theworldavatar.com/kb" # root URL
    
    topNode = "UK_Digital_Twin"  # Top level node
    
    powerPlant = "UK_power_plant" # Second level node
    energyConsumption = "UK_energy_consumption" # Second level node
    gridTopology = "UK_power_grid_topology" # Second level node
    powerGridModel = "UK_power_grid" # Second level node
    
    """ Raw data version""" # Third level node
    dukesDataVersion = {
        2019 : "operationalPowerPlantBy2019"
        }
    
    consumptionDataVersion = {
        2017: "energyConsumptionIn2017"
        }
    
    
    """ Model type """ # Third level node
    BusModel = {            
        1:   "1_bus_model", 
        10 : "10_bus_model"
        }
    
    """ Node identifiers(keys) """
    topLevelNode = 1
    secondLevelNode = 2
    thirdLevelNode = 3
    
    """NameSpace"""
    ukdigitaltwin = baseNode + SLASH + topNode
    
    
""" Named-graphs URI generator"""   
# 'namedGraphURIGenerator' is defined as a funciton belongs to the module 'UKDigitalTwin' instead of a method of class UKDigitalTwin
def namedGraphURIGenerator (nodeIdentifier, nodeName, dataOrModelVersion):
    if nodeIdentifier == UKDigitalTwin.topLevelNode and nodeName == UKDigitalTwin.topNode and dataOrModelVersion == None:
        _topNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.OWL + UKDigitalTwin.HASH + UKDigitalTwin.topNode
        return _topNode
    elif (nodeName == UKDigitalTwin.powerPlant or nodeName == UKDigitalTwin.energyConsumption\
          or nodeName == UKDigitalTwin.gridTopology or nodeName == UKDigitalTwin.powerGridModel):
        if nodeIdentifier == UKDigitalTwin.secondLevelNode and dataOrModelVersion == None:
              _secondLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.OWL + UKDigitalTwin.HASH + nodeName
              return _secondLevelNode
        elif nodeIdentifier == UKDigitalTwin.thirdLevelNode:
              if nodeName == UKDigitalTwin.powerPlant and dataOrModelVersion in UKDigitalTwin.dukesDataVersion.keys():
                  _thirdLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.dukesDataVersion[dataOrModelVersion]\
                  + UKDigitalTwin.OWL + UKDigitalTwin.HASH + UKDigitalTwin.dukesDataVersion[dataOrModelVersion]
                  return _thirdLevelNode
              elif nodeName == UKDigitalTwin.energyConsumption and dataOrModelVersion in UKDigitalTwin.consumptionDataVersion.keys():
                  _thirdLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.consumptionDataVersion[dataOrModelVersion]\
                  + UKDigitalTwin.OWL + UKDigitalTwin.HASH + UKDigitalTwin.consumptionDataVersion[dataOrModelVersion]
                  return _thirdLevelNode
              elif (nodeName == UKDigitalTwin.gridTopology or nodeName == UKDigitalTwin.powerGridModel)\
                        and dataOrModelVersion in UKDigitalTwin.BusModel.keys():
                  _thirdLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.BusModel[dataOrModelVersion]\
                      + UKDigitalTwin.OWL + UKDigitalTwin.HASH + UKDigitalTwin.BusModel[dataOrModelVersion]
                  return _thirdLevelNode
              else:
                  print ('Cannot construct the Named Graph (nodeLevel 3) URI, please check the input data')
        else:
            print ('Cannot construct the Named Graph (nodeLevel 2/3) URI, please check the input data') 
    else:
        print ('Cannot construct the Named Graph (nodeLevel 1/2/3) URI, please check the input data')
        return
    
if __name__ == '__main__':
    uri = namedGraphURIGenerator(3, UKDigitalTwin.powerPlant, 2019)
    print (uri.split('.owl'))



