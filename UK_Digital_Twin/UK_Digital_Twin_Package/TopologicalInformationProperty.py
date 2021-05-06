##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 05 May 2021          #
##########################################

"""This class defines the properties of UK Topological Information data"""

class TopologicalInformation:
    """ File path """
    DataPath = '../Data files/PowerGridTopology/'
    
    """Topology information and data path"""
    Topo_10_bus = {
        'Name' : '10_bus_topology', 
        'EBus_num' : 10,
        'ELine_num' : 14, 
        'BusInfo' : DataPath + '10_bus/bus_topological_info.csv',
        'BranchInfo': DataPath + '10_bus/branch_topological_info.csv'
        }
       
    """Data file header"""
    headerTopologicalInformation = ["BusNumber", "Region", "LargestCity", "y-axis", "x-axis\n"]
    
    """Source Data"""
    __ENCONSUMPT = DataPath +  "10_bus/-Node-24h-Tax-Auto - Template.xlsx"