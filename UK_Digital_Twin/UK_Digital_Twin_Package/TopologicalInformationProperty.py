##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 22 Sept 2021         #
##########################################

"""This class defines the properties of UK Topological Information data"""

class TopologicalInformation:
    
     def __init__(self, numOfBus = 10, numOfBranch = 14):
        """ File path """
        self.DataPath = '../Data files/PowerGridTopology/'
        self.EBus_num = numOfBus
        self.ELine_num = numOfBranch
        self.Name = str(self.EBus_num) + '_bus_topology'
        self.BusInfo =  self.DataPath + str(self.EBus_num) + '_bus/bus_topological_info.csv'
        self.BranchInfo =  self.DataPath + str(self.EBus_num) + '_bus/branch_topological_info.csv'
        self.BranchProperty =  self.DataPath + str(self.EBus_num) + '_bus/branch_properties.csv' # the branch prop should be calculated from the raw data
           
        """Data file header"""
        self.headerBusTopologicalInformation = ["BusNumber", "Region", "LocalAuthority", "x-axis", "y-axis", "Agrregated", "LACode\n"]
        self.headerBranchTopologicalInformation = ["FromBus", "ToBus", "400kV_PARALLEL_CONNECTIONS", "275kV_PARALLEL_CONNECTIONS\n"]
        self.headerBranchProperty = ["voltage_level_kV", "R_MVA/km", "X_MVA/km", "B_MVA/km", "MVA\n"]
        
        """Source Data"""
        if self.EBus_num == 10:
            self.__TOPOINFO = self.DataPath + str(self.EBus_num) +  "_bus/-Node-24h-Tax-Auto - Template.xlsx"
        if self.EBus_num == 29:
            self.DataSource = "https://www.maths.ed.ac.uk/optenergy/NetworkData/reducedGB/"
        

if __name__ == '__main__': 
    topoinfo = TopologicalInformation()
    print(topoinfo.Name)