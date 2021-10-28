##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 27 Oct 2021          #
##########################################

import sys, os
import numpy as np
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package.OWLfileStorer import readFile

"""This class is developed to provide different initialisation methods of the input variables of the branch model"""

class BranchPropertyInitialisation(object):
    
    """The default initialiser is firstly designed for the 10_bus model which by default using the values provided from the branchProperty csv file.
    By using this method, branchProperty must be provided. """ 
    def defaultBranchInitialiser(self, ELine_Model, ELineTopoAndGeometryInfo, branchVoltageLevel, counter):       
        if not isinstance (ELine_Model, UK_PG.UKElineModel):
            raise Exception('The ELine_Model argument should be an instence of UKElineModel.')
        if not os.path.exists(ELine_Model.BranchProperty):
            raise Exception('The document of BranchProperty does not exist.')
        
        branchPropertyArrays = readFile(ELine_Model.BranchProperty) 
        
        if branchPropertyArrays[0] != ELine_Model.headerBranchProperty:
            raise Exception('The header of BranchProperty does not match.')
        
        voltageLevelOfBranchProperty = [item[0].strip('\n') for item in branchPropertyArrays]
        del(voltageLevelOfBranchProperty[0])
        for voltage in branchVoltageLevel:            
            if not voltage.strip('kV') in voltageLevelOfBranchProperty:
                raise Exception('The voltage level of BranchProperty does not match.')
       
        ELine_Model.FROMBUS = int(ELineTopoAndGeometryInfo['From_Bus'].split('_EBus-')[1])
        ELine_Model.TOBUS = int(ELineTopoAndGeometryInfo['To_Bus'].split('_EBus-')[1])
        
        _branchPropertyArrays = np.array(branchPropertyArrays, dtype = object)
        
        # Specify the variables R, X, B and RateA of the ELine model
        R_inverse = 0
        X_inverse = 0
        B = 0
        RateA = 0
        for voltage in branchVoltageLevel:
            voltageLevelKey = "Num_OHL_" + voltage
            # index_voltageLevelOfBranchProperty
            i_V = np.argwhere(_branchPropertyArrays == voltage.strip('kV'))[0][0] 
            r = int(ELineTopoAndGeometryInfo[voltageLevelKey]) / (float(ELineTopoAndGeometryInfo['Value_Length_ELine']) * float(branchPropertyArrays[i_V][1]))
            x = int(ELineTopoAndGeometryInfo[voltageLevelKey]) / (float(ELineTopoAndGeometryInfo['Value_Length_ELine']) * float(branchPropertyArrays[i_V][2]))
            b = int(ELineTopoAndGeometryInfo[voltageLevelKey]) * float(ELineTopoAndGeometryInfo['Value_Length_ELine']) * float(branchPropertyArrays[i_V][3])
            rateA = float(branchPropertyArrays[i_V][4]) * int(ELineTopoAndGeometryInfo[voltageLevelKey])
            
            R_inverse += r            
            X_inverse += x
            B += b           
            RateA += rateA
            
        if R_inverse != 0: ELine_Model.R = 1/R_inverse
        if X_inverse != 0: ELine_Model.X = 1/X_inverse
        ELine_Model.B = B
        ELine_Model.RateA = RateA       
        return ELine_Model
    
    """The preSpecifiedBranchInitialiser is firstly designed for the 29_bus model which specify the values of the input for a set of given """ 
    def preSpecifiedBranchInitialiser(self, ELine_Model, ELineTopoAndGeometryInfo, branchVoltageLevel, counter):
        if not isinstance (ELine_Model, UK_PG.UKElineModel):
            raise Exception('The ELine_Model argument should be an instence of UKElineModel.')
            
        if not os.path.exists(ELine_Model.BranchModelInitialisation):
            raise Exception('The document of BranchModelInitialisation does not exist.')
         
        BranchModelInitialisationArrays = readFile(ELine_Model.BranchModelInitialisation)  
        
        if BranchModelInitialisationArrays[0] != ELine_Model.headerBranchProperty:
            raise Exception('The header of BranchProperty does not match.') 
          
        ELine_Model.FROMBUS = int(ELineTopoAndGeometryInfo['From_Bus'].split('_EBus-')[1])
        ELine_Model.TOBUS = int(ELineTopoAndGeometryInfo['To_Bus'].split('_EBus-')[1])
        
        if str(ELine_Model.FROMBUS) == BranchModelInitialisationArrays[counter][0].strip('\n') and \
            str(ELine_Model.TOBUS) == BranchModelInitialisationArrays[counter][1].strip('\n'):
            ELine_Model.R = BranchModelInitialisationArrays[counter][2].strip('\n')
            ELine_Model.X = BranchModelInitialisationArrays[counter][3].strip('\n')
            ELine_Model.B = BranchModelInitialisationArrays[counter][4].strip('\n')
            ELine_Model.RateA = BranchModelInitialisationArrays[counter][5].strip('\n')
            ELine_Model.RateB = BranchModelInitialisationArrays[counter][6].strip('\n')
            ELine_Model.RateC = BranchModelInitialisationArrays[counter][7].strip('\n')
            ELine_Model.RATIO = BranchModelInitialisationArrays[counter][8].strip('\n')
            ELine_Model.ANGLE = BranchModelInitialisationArrays[counter][9].strip('\n')
            ELine_Model.STATUS = BranchModelInitialisationArrays[counter][10].strip('\n')
            ELine_Model.ANGMIN = BranchModelInitialisationArrays[counter][11].strip('\n')
            ELine_Model.ANGMAX = BranchModelInitialisationArrays[counter][12].strip('\n')
        else:
            raise Exception('The ELine number does not match.') 
        return ELine_Model
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    