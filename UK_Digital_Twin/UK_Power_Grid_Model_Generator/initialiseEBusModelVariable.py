"""This class is designed to provide different initialise method for EBusModelVar"""

import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package.OWLfileStorer import readFile
import rfc3987
from rfc3987 import parse


class initialiseEBusModelVariable(object):
    
    """This method is the default initialisation"""
    ## the default method defines one slack bus and the rest of the buses are all of PQ type (load bus)
    def defaultInitialisation(self, EBus_Model_instance, EBus, BusNumber, slackBusNodeIRI):
        if not isinstance (EBus_Model_instance, UK_PG.UKEbusModel):
            raise Exception('The first argument should be an instence of UKEbusModel.')
            
        print('****The Bus model variables initialiser is defaultInitialisation****')
        EBus_Model_instance.BUS = int(BusNumber)
        
        parse(slackBusNodeIRI, rule="IRI")
       
        if EBus["BusNodeIRI"].strip("\n") == slackBusNodeIRI.strip("\n"): 
            print("...The slack bus is", slackBusNodeIRI)
            EBus_Model_instance.TYPE = 3
        else: 
            EBus_Model_instance.TYPE = 1
       
        EBus_Model_instance.PD_INPUT = round((float(EBus['v_TotalELecConsumption']) * 1000 / (24 * 365)), 3) 
        return EBus_Model_instance
    
    """This method uses the pre defined initailised values""" 
    def preSpecified(self, EBus_Model_instance, EBus, BusNumber, slackBusNodeIRI):
        if not isinstance (EBus_Model_instance, UK_PG.UKEbusModel):
            raise Exception('The first argument should be an instence of UKEbusModel.')
        BusModelInitialisationArrays = readFile(EBus_Model_instance.BusModelInitialisation)  
        if BusModelInitialisationArrays[0] != EBus_Model_instance.headerBusModel:
          raise Exception('The Bus Model Initialisation header is not matched, please check the data file')
        print('****The Bus model variables initialiser is preSpecified****')
        
        EBus_Model_instance.BUS = int(BusNumber)
        
        for busInput in BusModelInitialisationArrays:
            if str(EBus["BusLatLon"][0]) in str(busInput[14]) and str(EBus["BusLatLon"][1]) in str(busInput[13]):
                arrayIndex = BusModelInitialisationArrays.index(busInput)
                break
        
        EBus_Model_instance.TYPE = BusModelInitialisationArrays[arrayIndex][1].strip('\n')
        EBus_Model_instance.PD_INPUT = round((float(EBus['v_TotalELecConsumption']) * 1000 / (24 * 365)), 3) 
        EBus_Model_instance.GD_INPUT = BusModelInitialisationArrays[arrayIndex][3].strip('\n')
        EBus_Model_instance.GS = BusModelInitialisationArrays[arrayIndex][4].strip('\n')
        EBus_Model_instance.BS = BusModelInitialisationArrays[arrayIndex][5].strip('\n')
        EBus_Model_instance.AREA = BusModelInitialisationArrays[arrayIndex][6].strip('\n')
        EBus_Model_instance.VM_INPUT = BusModelInitialisationArrays[arrayIndex][7].strip('\n')
        EBus_Model_instance.VA_INPUT = BusModelInitialisationArrays[arrayIndex][8].strip('\n')
        EBus_Model_instance.BASEKV = BusModelInitialisationArrays[arrayIndex][9].strip('\n')
        EBus_Model_instance.ZONE = BusModelInitialisationArrays[arrayIndex][10].strip('\n')
        EBus_Model_instance.VMAX = BusModelInitialisationArrays[arrayIndex][11].strip('\n')
        EBus_Model_instance.VMIN = BusModelInitialisationArrays[arrayIndex][12].strip('\n')
        
        return EBus_Model_instance
if __name__ == '__main__':    
    iri = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_10fe8504-f3bb-403c-9363-34b258d59711"
    #iri = "asndnafiu"
    parse(iri, rule="IRI")
   
    
    
    
    
    
   
            

    
    
    