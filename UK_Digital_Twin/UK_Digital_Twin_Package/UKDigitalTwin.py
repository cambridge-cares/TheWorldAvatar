"""This module defines the sub-graph memebers of UK digital twin and the URI fragments used to construct the A-box"""

# Level of the node is different of the level of the knowledge graph. The level of the KG is depends on the degree of data being processed, 
# i. e. the data in Level 3 KG is processed based on the raw data from Level 1 and 3 
# Level 1 KG are powerPlant, energyConsumption; Level 2 KG contains gridTopology; Level 3 KG includes powerGridModel.

class UKDigitalTwin:
    
    """Notation used in URI construction"""
    HASH = '#'
    SLASH = '/'
    UNDERSCORE = '_'
    OWL = '.owl'
    
    """Location"""
    UK = "United_Kingdom"
    GB = "Great_Britain"
    NI = "Northern_Ireland"
    
    """ Node Names """
    baseURL = "http://www.theworldavatar.com/kb"    
    topNode = "ontoenergysystem" # Top level node
    
    powerPlant = "PowerPlant_" # Second level node
    electricitySystem = "ElectricPowerSystem_" # The newly addded second level node
    energyConsumption = "EnergyConsumption_" # Second level node
    gridTopology = "PowerGridTopology" # Second level node
    powerGridModel = "PowerGrid" # Second level node
    
    
    """ Model type """ # Third level node
    Model = {            
        1:   "1_bus_model", 
        10 : "10_bus_model",
        29 : "29_bus_model",
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
    
    """Root URL (Namespace)"""
    ukdigitaltwin = baseURL + SLASH + topNode
    
    
""" Named-graphs URI generator"""   
# 'namedGraphURIGenerator' is defined as a funciton belongs to the module 'UKDigitalTwin' instead of a method of class UKDigitalTwin
def nodeURIGenerator (nodeIdentifier, nodeName, dataOrModelVersion, subModelName = None):
    # print('nodeIdentifier',nodeIdentifier)
    # print('nodeName',nodeName)
    # print('dataOrModelVersion',dataOrModelVersion)
    # print('subModelName',subModelName)
    if nodeIdentifier == UKDigitalTwin.topLevelNode and nodeName == UKDigitalTwin.topNode and dataOrModelVersion == None:
        _topNode = UKDigitalTwin.ukdigitaltwin 
        return _topNode
    elif nodeName == UKDigitalTwin.powerPlant or nodeName == UKDigitalTwin.energyConsumption or nodeName == UKDigitalTwin.electricitySystem:
        _secondLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName
        return _secondLevelNode
    elif nodeName == UKDigitalTwin.gridTopology or nodeName == UKDigitalTwin.powerGridModel:
        if nodeIdentifier == UKDigitalTwin.secondLevelNode and dataOrModelVersion == None:
              _secondLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH
              return _secondLevelNode
        elif nodeIdentifier == UKDigitalTwin.thirdLevelNode:
              if (nodeName == UKDigitalTwin.gridTopology or nodeName == UKDigitalTwin.powerGridModel) and dataOrModelVersion in UKDigitalTwin.Model.keys():
                  _thirdLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.UNDERSCORE # + UKDigitalTwin.SLASH + UKDigitalTwin.Model[dataOrModelVersion] + UKDigitalTwin.SLASH
                  return _thirdLevelNode
              else:
                  raise Exception('Cannot construct the nodeLevel 3 URI, please check the input data.')
        
        elif nodeIdentifier == UKDigitalTwin.fourthLevelNode: # the return of the fourth level node is an array
              if nodeName == UKDigitalTwin.powerGridModel and dataOrModelVersion in UKDigitalTwin.Model.keys() and subModelName == None:
                  _fourthLevelNode = []
                  for submodel in UKDigitalTwin.SubModel.keys():
                      __fourthLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.Model[dataOrModelVersion]\
                          + UKDigitalTwin.SLASH + UKDigitalTwin.SubModel[submodel] + UKDigitalTwin.SLASH
                      _fourthLevelNode.append(__fourthLevelNode)
                  return _fourthLevelNode
              
              elif nodeName == UKDigitalTwin.powerGridModel and dataOrModelVersion in UKDigitalTwin.Model.keys() and subModelName in UKDigitalTwin.SubModel.keys():
                   _fourthLevelNode = UKDigitalTwin.ukdigitaltwin + UKDigitalTwin.SLASH + nodeName + UKDigitalTwin.SLASH + UKDigitalTwin.Model[dataOrModelVersion]\
                          + UKDigitalTwin.SLASH + UKDigitalTwin.SubModel[subModelName] + UKDigitalTwin.SLASH
                   return _fourthLevelNode
              else:
                  raise Exception ('Cannot construct the nodeLevel 4 URI, please check the input data') 
        else:
            raise Exception ('Cannot construct the nodeLevel 2/3 URI, please check the input data') 
    else:
        raise Exception ('Cannot construct the nodeLevel 1/2/3 URI, please check the input data')
    
if __name__ == '__main__':
    uri_topnode = nodeURIGenerator(1, UKDigitalTwin.topNode, None)    
    uri_pp = nodeURIGenerator(2, UKDigitalTwin.powerPlant, None)
    uri_ec = nodeURIGenerator(2, UKDigitalTwin.energyConsumption, None) 
    uri_es = nodeURIGenerator(2, UKDigitalTwin.electricitySystem, None) 
    uri_topo_10 = nodeURIGenerator(3, UKDigitalTwin.gridTopology, 10) 
    uri_topo_29 = nodeURIGenerator(3, UKDigitalTwin.gridTopology, 29) 
    uri_grid_10 = nodeURIGenerator(3, UKDigitalTwin.powerGridModel, 10) 
    uri_grid_29 = nodeURIGenerator(3, UKDigitalTwin.powerGridModel, 29) 
    
    uri_grid_10_model = nodeURIGenerator(4, UKDigitalTwin.powerGridModel, 10) 
    uri_grid_29_model = nodeURIGenerator(4, UKDigitalTwin.powerGridModel, 29) 
    
    uri_grid_10_model_Egen = nodeURIGenerator(4, UKDigitalTwin.powerGridModel, 10, "EGen")
    uri_grid_10_model_Eline = nodeURIGenerator(4, UKDigitalTwin.powerGridModel, 10, "ELine") 
    uri_grid_10_model_Ebus = nodeURIGenerator(4, UKDigitalTwin.powerGridModel, 10, "EBus") 
    
    ukElectricityConsumption = nodeURIGenerator(3, UKDigitalTwin.energyConsumption, 2017)
    print(uri_pp, uri_topo_10)
    # print(uri_grid_10_model_Egen, uri_grid_10_model_Eline, uri_grid_10_model_Ebus )
    
    
    # print (uri_topnode, uri_pp, uri_ec, uri_es, uri_topo_10, uri_topo_29, uri_grid_10, uri_grid_29, uri_grid_10_model, uri_grid_29_model)



