##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 01 Dec 2021          #
##########################################

"""This class is designed to provide different initialise method for EBusModelVar"""
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package.OWLfileStorer import readFile

class initialiseEBusModelVariable(object):
    
    """This method is the default initialisation"""
    def defaultInitialisation(self, EBus_Model_instance, EBus):
        if not isinstance (EBus_Model_instance, UK_PG.UKEbusModel):
            raise Exception('The first argument should be an instence of UKEbusModel.')
        print('****The Bus model variables initialiser is defaultInitialisation****')
        EBus_Model_instance.BUS = int((EBus['EBus'].split('#EBus-')[1]).split('_')[0])
        if EBus_Model_instance.BUS == 1: # assign slack bus
            EBus_Model_instance.TYPE = 3
        EBus_Model_instance.PD_INPUT = round((float(EBus['v_TotalELecConsumption']) * 1000 / (24 * 365)), 3) 
        return EBus_Model_instance
    
    """This method uses the pre defined initailised values""" 
    def preSpecified(self, EBus_Model_instance, EBus):
        if not isinstance (EBus_Model_instance, UK_PG.UKEbusModel):
            raise Exception('The first argument should be an instence of UKEbusModel.')
        BusModelInitialisationArrays = readFile(EBus_Model_instance.BusModelInitialisation)  
        if BusModelInitialisationArrays[0] != EBus_Model_instance.headerBusModel:
          raise Exception('The Bus Model Initialisation header is not matched, please check the data file')
        print('****The Bus model variables initialiser is preSpecified****')
        EBus_Model_instance.BUS = int((EBus['EBus'].split('#EBus-')[1]).split('_')[0])
        EBus_Model_instance.TYPE = BusModelInitialisationArrays[EBus_Model_instance.BUS][1].strip('\n')
        EBus_Model_instance.PD_INPUT = BusModelInitialisationArrays[EBus_Model_instance.BUS][2].strip('\n')
        EBus_Model_instance.GD_INPUT = BusModelInitialisationArrays[EBus_Model_instance.BUS][3].strip('\n')
        EBus_Model_instance.GS = BusModelInitialisationArrays[EBus_Model_instance.BUS][4].strip('\n')
        EBus_Model_instance.BS = BusModelInitialisationArrays[EBus_Model_instance.BUS][5].strip('\n')
        EBus_Model_instance.AREA = BusModelInitialisationArrays[EBus_Model_instance.BUS][6].strip('\n')
        EBus_Model_instance.VM_INPUT = BusModelInitialisationArrays[EBus_Model_instance.BUS][7].strip('\n')
        EBus_Model_instance.VA_INPUT = BusModelInitialisationArrays[EBus_Model_instance.BUS][8].strip('\n')
        EBus_Model_instance.BASEKV = BusModelInitialisationArrays[EBus_Model_instance.BUS][9].strip('\n')
        EBus_Model_instance.ZONE = BusModelInitialisationArrays[EBus_Model_instance.BUS][10].strip('\n')
        EBus_Model_instance.VMAX = BusModelInitialisationArrays[EBus_Model_instance.BUS][11].strip('\n')
        EBus_Model_instance.VMIN = BusModelInitialisationArrays[EBus_Model_instance.BUS][12].strip('\n')
        
        EBus_Model_instance.PD_INPUT = round((float(EBus['v_TotalELecConsumption']) * 1000 / (24 * 365)), 3) 
          
        return EBus_Model_instance
