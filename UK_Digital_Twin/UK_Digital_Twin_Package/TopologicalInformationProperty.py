##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 22 Sept 2021         #
##########################################

"""This class defines the properties of UK Topological Information data"""
from pathlib import Path

class TopologicalInformation:
    
     def __init__(self, numOfBus:int, voltageLevel = ["275", "400"]):
        """ File path """
        self.DataPath = str(Path(__file__).resolve().parent.parent) + "/Data files/PowerGridTopology/"
        self.EBus_num = numOfBus
        self.Name = str(self.EBus_num) + '_bus_topology'
        self.BusInfo =  self.DataPath + str(self.EBus_num) + '_bus/bus_topological_info.csv'
        self.BranchInfo =  self.DataPath + str(self.EBus_num) + '_bus/branch_topological_info.csv'

        # for 29 bus model, the IRI of the branch needs to be recorded in the BranchModelInitialisation file
        self.BranchModelInitialisation = str(Path(__file__).resolve().parent.parent) + "/Data files/PowerGridModelInitialisation/" + str(numOfBus) + '_bus/BranchModelInitialisation.csv'     
        self.UpdateHeaderOfBranchModelInitialisation = "BranchNodeIRI"
        
        # self.BranchProperty =  self.DataPath + str(self.EBus_num) + '_bus/branch_properties.csv' # the branch prop should be calculated from the raw data
           
        """Data file header"""
        self.voltageLevel = voltageLevel
        self.headerBusTopologicalInformation = ["BusNumber", "Region", "LocalAuthority", "x-axis", "y-axis", "Aggregated\n"]
        
        self.headerBranchTopologicalInformation = ["FromBus", "ToBus", "BranchNodeIRI"] # the 400 and 275 refers tp the PARALLEL_CONNECTIONS       
        for voltage in self.voltageLevel:
             self.headerBranchTopologicalInformation.append(voltage)
                
        """Source Data"""
        if self.EBus_num == 10:
            self.__TOPOINFO = self.DataPath + str(self.EBus_num) +  "_bus/-Node-24h-Tax-Auto - Template.xlsx"
        if self.EBus_num == 29:
            self.DataSource = "https://www.maths.ed.ac.uk/optenergy/NetworkData/reducedGB/"