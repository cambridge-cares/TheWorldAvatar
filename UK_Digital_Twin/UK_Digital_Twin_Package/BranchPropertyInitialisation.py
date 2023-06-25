##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 12 Oct 2022          #
##########################################

import sys, os, json
import numpy as np
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from UK_Digital_Twin_Package.OWLfileStorer import readFile
from UK_Digital_Twin_Package.queryInterface import performQuery
from logging import raiseExceptions
from rfc3987 import parse

"""This class is developed to provide different initialisation methods of the input variables of the branch model"""

class BranchPropertyInitialisation(object):
    
    """The default initialiser is firstly designed for the 10_bus model which by default using the values provided from the branchProperty csv file.
    By using this method, branchProperty must be provided. """ 
    def defaultBranchInitialiser(self, ELineNodeIRI, ELine_Model, ELineTopoAndGeometryInfo, branchVoltageLevel, OrderedBusNodeIRIList, endpoint_label):       
        if not isinstance (ELine_Model, UK_PG.UKElineModel):
            raise Exception('The ELine_Model argument should be an instence of UKElineModel.')
        if not os.path.exists(ELine_Model.BranchProperty):
            raise Exception('The document of BranchProperty does not exist.')
        
        branchPropertyArrays = readFile(ELine_Model.BranchProperty) 
        
        if branchPropertyArrays[0] != ELine_Model.headerBranchProperty:
            raise Exception('The header of BranchProperty does not match.')
        
        ## check if the voltage level is consistent 
        voltageLevelOfBranchProperty = [item[0].strip('\n') for item in branchPropertyArrays]
        del(voltageLevelOfBranchProperty[0]) # delete the the header of the file
        for voltage in branchVoltageLevel:            
            if not voltage.strip('kV') in voltageLevelOfBranchProperty:
                raise Exception('The voltage level of BranchProperty does not match.')
  
        ## identify the number of the frombus and tobus of the current branch
        ELine_Model.FROMBUS = int(OrderedBusNodeIRIList.index(ELineTopoAndGeometryInfo['From_Bus']))
        ELine_Model.TOBUS = int(OrderedBusNodeIRIList.index(ELineTopoAndGeometryInfo['To_Bus']))
        
        ## convert branchPropertyArrays into nump array
        _branchPropertyArrays = np.array(branchPropertyArrays, dtype = object)
        
        ## Specify the variables R, X, B and RateA of the ELine model
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
        ELine_Model.RateB = RateA 
        ELine_Model.RateC = RateA 

        return ELine_Model
    
    """The preSpecifiedBranchInitialiser is firstly designed for the 29_bus model which specify the values of the input for a set of given """ 
    def preSpecifiedBranchInitialiser(self, ELineNodeIRI, ELine_Model, ELineTopoAndGeometryInfo, branchVoltageLevel, OrderedBusNodeIRIList, endpoint_label):
        if not isinstance (ELine_Model, UK_PG.UKElineModel):
            raise Exception('The ELine_Model argument should be an instence of UKElineModel.')
            
        if not os.path.exists(ELine_Model.BranchModelInitialisation):
            raise Exception('The document of BranchModelInitialisation does not exist.')
        
        ## read the files from local folder
        BranchModelInitialisationArrays = readFile(ELine_Model.BranchModelInitialisation)  
        branchTopoInfoArrays = readFile(ELine_Model.BranchInfo)
        branchTopoInfoArrays = np.array(branchTopoInfoArrays)

        for iri in branchTopoInfoArrays[1:,2]:
            parse(iri, rule="IRI") # check if the iri is a valid IRI
            iri.strip("\n").strip(" ")

        ## valid the header of the BranchModelInitialisationArrays
        if BranchModelInitialisationArrays[0] != ELine_Model.headerBranchProperty:
            raise Exception('The header of BranchProperty does not match.') 
        
        ## valid the branchNodeIRI
        branchNodeIRI = branchTopoInfoArrays[1][2]      
        if not self.branchNodeIRIExitInRemoteStore(endpoint_label, branchNodeIRI): 
            raise Exception('!!!!The branchNodeIRI does not exist in the remote store, please update the triples!!!!')

        print("---The given branch node IRI is in the remote store---")

        if len(BranchModelInitialisationArrays) != len(branchTopoInfoArrays):
            raise Exception('!!!!The number of the rows of BranchModelInitialisationArrays does not equal to branchTopoInfoArrays!!!!')
          
        ELine_Model.FROMBUS = int(OrderedBusNodeIRIList.index(ELineTopoAndGeometryInfo['From_Bus']))
        ELine_Model.TOBUS = int(OrderedBusNodeIRIList.index(ELineTopoAndGeometryInfo['To_Bus']))

        for iri in branchTopoInfoArrays[:,2]:
            if str(ELineNodeIRI) in str(iri):
                counter = int(np.where(branchTopoInfoArrays[:,2] == iri)[0][0])

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
        
        return ELine_Model

    """This method is called to check if the Branch Node exists in the remote triple store""" 
    def branchNodeIRIExitInRemoteStore(self, endpoint_label, branchNodeIRI): 
        if endpoint_label == str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['label']):
            endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['endpoint_iri'])
        elif parse(endpoint_label, rule='IRI'):
            endPointIRI = endpoint_label
        else:
            raiseExceptions("!!!!Please provide a valid endpoint!!!!")
        queryStr = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
        ASK  { <%s> rdf:type ontopowsys_PowSysRealization:ElectricalLine . 
        }""" %str(branchNodeIRI).strip('\n').strip('')

        print('...checking the existing of the branch node iri...')
        exitFlag = json.loads(performQuery(endPointIRI, queryStr))
        return exitFlag[0]['ASK']

if __name__ == '__main__':           
    bpi = BranchPropertyInitialisation()
    test = bpi.branchNodeIRIExitInRemoteStore("ukdigitaltwin_test2","http://www.theworldavatar.com/kb/ontopowsys/OverheadLine_765dee93-6780-4182-9e93-fdad9e5b9de2")
    print(test)