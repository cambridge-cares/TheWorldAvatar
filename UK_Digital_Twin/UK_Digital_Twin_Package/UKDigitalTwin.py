##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 June 2021         #
##########################################

"""This module defines the sub-graph memebers of UK digital twin and the URI fragments used to construct the A-box"""

# Level of the node is different of the level of the knowledge graph. The level of the KG is depends on the degree of data being processed, 
# i. e. the data in Level 3 KG is processed based on the raw data from Level 1 and 3 
# Level 1 KG are powerPlant, energyConsumption; Level 2 KG contains gridTopology; Level 3 KG includes powerGridModel.

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLable

class UKDigitalTwin:
    
    """Notation used in URI construction"""
    HASH = '#'
    SLASH = '/'
    UNDERSCORE = '_'
    OWL = '.owl'
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\Top_node\\"
    
    """Default path of SleepycatStoragePath"""
    SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\Top_node\\Sleepycat_topnode"
    
    """Default remote endpoint"""
    endpoint = EndPointConfigAndBlazegraphRepoLable.UKDigitalTwinKG
    
    """ Node Names """
    baseURL = "http://www.theworldavatar.com/kb" 
    
    topNode = "UK_Digital_Twin"  # Top level node
    
    powerPlant = "UK_power_plant" # Second level node
    energyConsumption = "UK_energy_consumption" # Second level node
    gridTopology = "UK_power_grid_topology" # Second level node
    powerGridModel = "UK_power_grid" # Second level node
    
    """ Raw data version""" # Third level node
    dukesDataVersion = {
        2019 : "operationalPowerPlantBy2019",
        9999: "operationalPowerPlantBy9999",
        }
    
    consumptionDataVersion = {
        2017: "energyConsumptionIn2017",
		9999: "energyConsumptionIn9999"
        }
    
    
    """ Model type """ # Third level node
    Model = {            
        1:   "1_bus_model", 
        10 : "10_bus_model"
        }
    
    """Model_EGen, Model_EBus, Model_ELine""" # fourth level nodes
    SubModel = {
        "EGen": "Model_EGen",
        "EBus": "Model_EBus",
        "ELine": "Model_ELine"
        }    
    
    """ Node identifiers(keys) """
    topLevelNode = 1
    secondLevelNode = 2
    thirdLevelNode = 3
    fourthLevelNode = 4
    
    """Root URL"""
    ukdigitaltwin = baseURL + SLASH + topNode
    
    
""" Named-graphs URI generator"""   
# 'namedGraphURIGenerator' is defined as a funciton belongs to the module 'UKDigitalTwin' instead of a method of class UKDigitalTwin
def nodeURIGenerator (nodeIdentifier, nodeName, dataOrModelVersion, subModelName = None):
    print('nodeIdentifier',nodeIdentifier)
    print('nodeName',nodeName)
    print('dataOrModelVersion',dataOrModelVersion)
    print('subModelName',subModelName)
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
                  print('_thirdLevelNode', _thirdLevelNode)
                  return _thirdLevelNode
              elif nodeName == UKDigitalTwin.energyConsumption and dataOrModelVersion in UKDigitalTwin.consumptionDataVersion.keys():
                  _thirdLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.consumptionDataVersion[dataOrModelVersion]\
                  + UKDigitalTwin.OWL + UKDigitalTwin.HASH + UKDigitalTwin.consumptionDataVersion[dataOrModelVersion]
                  return _thirdLevelNode
              elif (nodeName == UKDigitalTwin.gridTopology or nodeName == UKDigitalTwin.powerGridModel)\
                        and dataOrModelVersion in UKDigitalTwin.Model.keys():
                  _thirdLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.Model[dataOrModelVersion]\
                      + UKDigitalTwin.OWL + UKDigitalTwin.HASH + UKDigitalTwin.Model[dataOrModelVersion]
                  return _thirdLevelNode
              else:
                  print ('Cannot construct the nodeLevel 3 URI, please check the input data')
                  
        elif nodeIdentifier == UKDigitalTwin.fourthLevelNode: # the return of the fourth level node is an array
              if nodeName == UKDigitalTwin.powerGridModel and dataOrModelVersion in UKDigitalTwin.Model.keys() and subModelName==None:
                  _fourthLevelNode = []
                  for submodel in UKDigitalTwin.SubModel.keys():
                      __fourthLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.Model[dataOrModelVersion]\
                          + UKDigitalTwin.SLASH + UKDigitalTwin.SubModel[submodel] + UKDigitalTwin.OWL + UKDigitalTwin.HASH + UKDigitalTwin.SubModel[submodel] 
                      _fourthLevelNode.append(__fourthLevelNode)
                  return _fourthLevelNode
              
              elif nodeName == UKDigitalTwin.powerGridModel and dataOrModelVersion in UKDigitalTwin.Model.keys() and subModelName in UKDigitalTwin.SubModel.keys():
                   _fourthLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.Model[dataOrModelVersion]\
                          + UKDigitalTwin.SLASH + UKDigitalTwin.SubModel[subModelName] + UKDigitalTwin.OWL + UKDigitalTwin.HASH + UKDigitalTwin.SubModel[subModelName] 
                   return _fourthLevelNode
              else:
                  print ('Cannot construct the nodeLevel 4 URI, please check the input data') 
        else:
            print ('Cannot construct the nodeLevel 2/3 URI, please check the input data') 
    else:
        print ('Cannot construct the nodeLevel 1/2/3 URI, please check the input data')
        return
    
if __name__ == '__main__':
    uri = nodeURIGenerator(4, UKDigitalTwin.powerGridModel, 10)
    print (uri)



