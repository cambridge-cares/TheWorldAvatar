##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 06 May 2021          #
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
        'BranchInfo': DataPath + '10_bus/branch_topological_info.csv',
        'BranchProperty': DataPath + '10_bus/branch_properties.csv' # the branch prop should be calculated from the raw data
        }
       
    """Data file header"""
    headerBusTopologicalInformation = ["BusNumber", "Region", "LargestCity", "x-axis", "y-axis", "Agrregated\n"]
    headerBranchTopologicalInformation = ["FromBus", "ToBus", "400kV_PARALLEL_CONNECTIONS", "275kV_PARALLEL_CONNECTIONS\n"]
    headerBranchProperty = ["voltage_level_kV", "R_MVA/km", "X_MVA/km", "B_MVA/km", "MVA\n"]
    
    """Source Data"""
    __TOPOINFO = DataPath +  "10_bus/-Node-24h-Tax-Auto - Template.xlsx"