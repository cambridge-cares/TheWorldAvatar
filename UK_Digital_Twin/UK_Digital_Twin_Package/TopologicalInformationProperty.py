##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 22 Sept 2021         #
##########################################

"""This class defines the properties of UK Topological Information data"""

class TopologicalInformation:
    
     def __init__(self, numOfBus = 10, numOfBranch = 14, voltageLevel = ["275", "400"]):
        """ File path """
        self.DataPath = '../Data files/PowerGridTopology/'
        self.EBus_num = numOfBus
        self.ELine_num = numOfBranch
        self.Name = str(self.EBus_num) + '_bus_topology'
        self.BusInfo =  self.DataPath + str(self.EBus_num) + '_bus/bus_topological_info.csv'
        self.BranchInfo =  self.DataPath + str(self.EBus_num) + '_bus/branch_topological_info.csv'
        # self.BranchProperty =  self.DataPath + str(self.EBus_num) + '_bus/branch_properties.csv' # the branch prop should be calculated from the raw data
           
        """Data file header"""
        self.voltageLevel = voltageLevel
        self.headerBusTopologicalInformation = ["BusNumber", "Region", "LocalAuthority", "x-axis", "y-axis", "Agrregated\n"]
        
        self.headerBranchTopologicalInformation = ["FromBus", "ToBus"] # the 400 and 275 refers tp the PARALLEL_CONNECTIONS       
        for voltage in self.voltageLevel:
             self.headerBranchTopologicalInformation.append(voltage)
                
        """Source Data"""
        if self.EBus_num == 10:
            self.__TOPOINFO = self.DataPath + str(self.EBus_num) +  "_bus/-Node-24h-Tax-Auto - Template.xlsx"
        if self.EBus_num == 29:
            self.DataSource = "https://www.maths.ed.ac.uk/optenergy/NetworkData/reducedGB/"
        

if __name__ == '__main__': 
    topoinfo = TopologicalInformation(10, 14, [])
    print(topoinfo.headerBranchTopologicalInformation)
