##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 17 March 2022        #
##########################################

"""This module declare the properties of generating UK power grid model A-boxes"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel

from pypower import idx_bus
from pypower import idx_brch
from pypower import idx_gen 
import collections

valueKey = "value_"

"""Default remote endpoint"""
endpoint = EndPointConfigAndBlazegraphRepoLabel.UKPowerGridModelKG

class UKEbusModel:
    
    """Data path"""
    DataPath = '../Data files/PowerGridModelInitialisation/'
    
    """EBus Node keys"""
    EBusKey = "EBus-"
    ModelEBusKey = "Model_EBus-"
    
    """Model variable keys"""
    BUSNUMKey = "BusNumber_"
    BUSTYPEKey = "BusType_"
    PD_INPUTKey = "InputVariable_Pd_"
    GD_INPUTKey = "InputVariable_Gd_"
    PD_OUTPUTKey = "StateVariable_Pd_"
    GD_OUTPUTKey = "StateVariable_Gd_"
    GSKey = "Gs_"
    BSKey = "Bs_"
    AREAKey = "Area_"
    VM_INPUTKey = "InputVariable_Vm_"
    VA_INPUTKey = "InputVariable_Va_"
    VM_OUTPUTKey = "StateVariable_Vm_"
    VA_OUTPUTKey = "StateVariable_Va_"
    BASEKVKey = "BaseKV_"
    ZONEKey = "Zone_"
    VMAXKey = "VmMax_"
    VMINKey = "VmMin_"
    PDGENKey = "Pd_Gen_"
    GDGENKey = "Gd_Gen_"
    
    """Data file header"""
    headerBusModel = ["Bus", "Type", "Pd", "Gd", "Gs", "Bs", "area", "Vm", "Va", "basekV", "zone", "Vmax", "Vmin\n"]
    
    # Map with the bus indices of the pypower package
    
    """The dictionary index key for PF analysis """
   # INPUT_VARIABLE = ["BUS", "TYPE", "PD_INPUT", "GD_INPUT", "GS", "BS", "AREA", "VM_INPUT", "VA_INPUT", "BASEKV", "ZONE", "VMAX", "VMIN"]
    OUTPUT_VARIABLE = ["VM_OUTPUT", "VA_OUTPUT", "PDGEN", "GDGEN", "PD_OUTPUT", "GD_OUTPUT"]
    
    # Map keys with the bus indices of the pypower package (keys are the attribute named defined in UK digital twin and the value is the index used in the pyPower)
    INPUT_VARIABLE = collections.OrderedDict()
    INPUT_VARIABLE["BUS"] =  idx_bus.BUS_I 
    INPUT_VARIABLE["TYPE"] =  idx_bus.BUS_TYPE 
    INPUT_VARIABLE["PD_INPUT"] =  idx_bus.PD 
    INPUT_VARIABLE["GD_INPUT"] =  idx_bus.QD 
    INPUT_VARIABLE["GS"] =  idx_bus.GS
    INPUT_VARIABLE["BS"] =  idx_bus.BS
    INPUT_VARIABLE["AREA"] =  idx_bus.BUS_AREA
    INPUT_VARIABLE["VM_INPUT"] =  idx_bus.VM
    INPUT_VARIABLE["VA_INPUT"] =  idx_bus.VA
    INPUT_VARIABLE["BASEKV"] =  idx_bus.BASE_KV
    INPUT_VARIABLE["ZONE"] =  idx_bus.ZONE
    INPUT_VARIABLE["VMAX"] =  idx_bus.VMAX
    INPUT_VARIABLE["VMIN"] =  idx_bus.VMIN
    
    INPUT_VARIABLE_KEYS = list(INPUT_VARIABLE.keys())
    
    # {"BUS":"BUS_I", "TYPE": "BUS_TYPE", "PD_INPUT": "PD", "GD_INPUT": "QD", "GS": "GS", "BS": "BS", "AREA": "BUS_AREA", "VM_INPUT": "VM", "VA_INPUT": "VA", "BASEKV": "BASE_KV", "ZONE": "ZONE", "VMAX":"VMAX", "VMIN": "VMIN" }
    
    def __init__(self, DUKESVersion = 2019, numOfBus = 10, Location = 'http://dbpedia.org/resource/United_Kingdom'):
        self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EBus\\"
        self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EBus\\Sleepycat_EBus"
        self.BusModelInitialisation = UKEbusModel.DataPath + str(numOfBus) + '_bus/BusModelInitialisation.csv'        
        self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EBus\\Sleepycat_EBus"
        self.DUKESVersion = DUKESVersion
        self.numOfBus = numOfBus
        self.location = Location
        
        # Model input
        self.INPUT_VARIABLE_KEYS[0] = None # BUS
        self.INPUT_VARIABLE_KEYS[1] = 1 # TYPE
        self.INPUT_VARIABLE_KEYS[2] = None # PD_INPUT
        self.INPUT_VARIABLE_KEYS[3] = 0 # GD_INPUT
        self.INPUT_VARIABLE_KEYS[4] = 0 # GS
        self.INPUT_VARIABLE_KEYS[5] = 0 # BS
        self.INPUT_VARIABLE_KEYS[6] = 1 # AREA 
        self.INPUT_VARIABLE_KEYS[7] = 1 # VM_INPUT
        self.INPUT_VARIABLE_KEYS[8] = 0 # VA_INPUT            
        self.INPUT_VARIABLE_KEYS[9] = 400 # BASEKV
        self.INPUT_VARIABLE_KEYS[10] = 1 # ZONE
        self.INPUT_VARIABLE_KEYS[11] = 1.05 # VMAX
        self.INPUT_VARIABLE_KEYS[12] = 0.95 # VMIN
        
        # Model output
        self.PD_OUTPUT = None
        self.GD_OUTPUT = None
        self.VM_OUTPUT = None
        self.VA_OUTPUT = None  
        self.PDGEN = None
        self.GDGEN = None
        
        
        
    def __dir__(self):
        return {"INPUT": self.INPUT_VARIABLE, "OUTPUT": self.OUTPUT_VARIABLE}
         

class UKElineModel:
    
    """Data path"""
    DataPath = '../Data files/PowerGridModelInitialisation/'
    
    """ELine Node keys"""
    ELineKey = "ELine-"
    ModelELineKey = "Model_ELine-"
    
    """Model variable keys"""
    FROMBUSKey = "FromBusNumber_"
    TOBUSKey = "ToBusNumber_"
    R_Key = "R_"
    X_Key = "X_"
    B_Key = "B_"
    RateAKey = "RateA_"
    RateBKey = "RateB_"
    RateCKey = "RateC_"
    RATIOKey = "RatioCoeff_"
    ANGLEKey = "Angle_"
    STATUSKey = "Status_"
    ANGMINKey = "AngleMin_"
    ANGMAXKey = "AngleMax_"
    
    """Data file header"""
    # headerBranchModel = ["Bus1", "Bus2", "R", "X", "B", "RateA", "RateB", "RateC", "ratio", "angle", "status", "angmin", "angmax\n"]
    
    """The INPUT_VARIABLE and OUTPUT_VARIABLE index key for PF analysis """    
    # INPUT_VARIABLE = ["FROMBUS", "TOBUS", "R", "X", "B", "RateA", "RateB", "RateC", "RATIO", "ANGLE", "STATUS", "ANGMIN", "ANGMAX"]
    OUTPUT_VARIABLE = ["FROMBUSINJECTION_P", "FROMBUSINJECTION_Q", "TOBUSINJECTION_P", "TOBUSINJECTION_Q", "LOSS_P", "LOSS_Q"]
    
    """Mapping the index key from PyPower"""
    INPUT_VARIABLE = collections.OrderedDict()
    INPUT_VARIABLE["FROMBUS"] =  idx_brch.F_BUS 
    INPUT_VARIABLE["TOBUS"] =  idx_brch.T_BUS 
    INPUT_VARIABLE["R"] =  idx_brch.BR_R 
    INPUT_VARIABLE["X"] =  idx_brch.BR_X 
    INPUT_VARIABLE["B"] =  idx_brch.BR_B
    INPUT_VARIABLE["RateA"] =  idx_brch.RATE_A
    INPUT_VARIABLE["RateB"] =  idx_brch.RATE_B
    INPUT_VARIABLE["RateC"] =  idx_brch.RATE_C
    INPUT_VARIABLE["RATIO"] =  idx_brch.TAP
    INPUT_VARIABLE["ANGLE"] =  idx_brch.SHIFT
    INPUT_VARIABLE["STATUS"] =  idx_brch.BR_STATUS
    INPUT_VARIABLE["ANGMIN"] =  idx_brch.ANGMIN
    INPUT_VARIABLE["ANGMAX"] =  idx_brch.ANGMAX
    
    INPUT_VARIABLE_KEYS = list(INPUT_VARIABLE.keys())
    
    def __init__(self, DUKESVersion = 2019, numOfBus = 10, initialiserMethod = 'defaultBranchInitialiser', Location = 'http://dbpedia.org/resource/United_Kingdom'):
        
        self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\ELine\\"
        self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\ELine\\Sleepycat_EBus"
        self.BranchProperty =  UKElineModel.DataPath + str(numOfBus) + '_bus/branch_properties.csv' # the branch prop should be calculated from the raw data
        self.BranchModelInitialisation = UKElineModel.DataPath + str(numOfBus) + '_bus/BranchModelInitialisation.csv'             
        if initialiserMethod == 'defaultBranchInitialiser':
            self.headerBranchProperty = ["voltage_level_kV", "R_MVA/km", "X_MVA/km", "B_MVA/km", "MVA\n"]
        elif initialiserMethod == 'preSpecifiedBranchInitialiser':
            self.headerBranchProperty = ["Bus1", "Bus2", "R", "X", "B", "RateA", "RateB", "RateC", "ratio",	"angle", "status", "angmin", "angmax\n"]
        else:
            self.headerBranchProperty = []
        
        self.DUKESVersion = DUKESVersion
        self.numOfBus = numOfBus
        self.location =  Location
        
        ## Attributes ##
        # model input
        self.INPUT_VARIABLE_KEYS[0] = None # FROMBUS
        self.INPUT_VARIABLE_KEYS[1] = None # TOBUS
        self.INPUT_VARIABLE_KEYS[2] = None # R
        self.INPUT_VARIABLE_KEYS[3] = None # X
        self.INPUT_VARIABLE_KEYS[4] = None # B
        self.INPUT_VARIABLE_KEYS[5] = None # RateA
        self.INPUT_VARIABLE_KEYS[6] = 0 # RateB
        self.INPUT_VARIABLE_KEYS[7] = 0 # RateC
        self.INPUT_VARIABLE_KEYS[8] = 1 # RATIO
        self.INPUT_VARIABLE_KEYS[9] = 0 # ANGLE
        self.INPUT_VARIABLE_KEYS[10] = 1 # STATUS
        self.INPUT_VARIABLE_KEYS[11] = -360 # ANGMIN
        self.INPUT_VARIABLE_KEYS[12] = 360 # ANGMAX
        
        # The PF analysis output
        self.FROMBUSINJECTION_P = None
        self.FROMBUSINJECTION_Q = None
        self.TOBUSINJECTION_P = None
        self.TOBUSINJECTION_Q = None
        self.LOSS_P = None
        self.LOSS_Q = None
    
    def __dir__(self):
        return {"INPUT": self.INPUT_VARIABLE, "OUTPUT": self.OUTPUT_VARIABLE}       

class UKEGenModel:
    
    """EGen Node keys"""
    EGenKey = "EGen-"
    ModelEGenKey = "Model_EGen-"
    
    """Model variable keys"""
    BUSNUMKey = "BusNumber_"
    PG_INPUTKey = "InputVariable_PGen_"
    QG_INPUTKey = "InputVariable_QGen_"
    PG_OUTPUTKey = "StateVariable_PGen_"
    QG_OUTPUTKey = "StateVariable_QGen_"
    QMAXKey = "QMax_"
    QMINKey = "QMin_"
    VGKey = "Vg_"
    MBASEKey = "mBase_"
    STATUSKey ="Status_"
    PMAXKey = "Pmax_"
    PMINKey = "Pmin_"
    PC1Key = "Pc1_"
    PC2Key = "Pc2_"
    QC1MINKey = "Qc1Min_"
    QC2MINKey = "Qc2Min_"
    QC1MAXKey = "Qc1Max_"
    QC2MAXKey = "Qc2Max_"
    RAMP_AGCKey = "Ramp_agc_"
    RAMP_10Key = "Ramp_10_"
    RAMP_30Key = "Ramp_30_"
    RAMP_QKey = "Ramp_q_"
    APFKey = "APF_"
    
    """The INPUT_VARIABLE and OUTPUT_VARIABLE index key for PF analysis """   
    # INPUT_VARIABLE = ["BUS", "PG_INPUT", "QG_INPUT", "QMAX", "QMIN", "VG", "MBASE", "STATUS", "PMAX", "PMIN", "PC1", "PC2", "QC1MIN", "QC1MAX", "QC2MIN", "QC2MAX", "RAMP_AGC", \
    #                   "RAMP_10", "RAMP_30", "RAMP_Q", "APF"]
    
    OUTPUT_VARIABLE = ["PG_OUTPUT", "QG_OUTPUT"]
    
    """Mapping the index key from PyPower"""
    INPUT_VARIABLE = collections.OrderedDict()
    INPUT_VARIABLE["BUS"] =  idx_gen.GEN_BUS 
    INPUT_VARIABLE["PG_INPUT"] =  idx_gen.PG
    INPUT_VARIABLE["QG_INPUT"] =  idx_gen.QG 
    INPUT_VARIABLE["QMAX"] =  idx_gen.QMAX 
    INPUT_VARIABLE["QMIN"] =  idx_gen.QMIN
    INPUT_VARIABLE["VG"] =  idx_gen.VG
    INPUT_VARIABLE["MBASE"] =  idx_gen.MBASE
    INPUT_VARIABLE["STATUS"] =  idx_gen.GEN_STATUS
    INPUT_VARIABLE["PMAX"] =  idx_gen.PMAX
    INPUT_VARIABLE["PMIN"] =  idx_gen.PMIN
    INPUT_VARIABLE["PC1"] =  idx_gen.PC1
    INPUT_VARIABLE["PC2"] =  idx_gen.PC2
    INPUT_VARIABLE["QC1MIN"] =  idx_gen.QC1MIN    
    INPUT_VARIABLE["QC1MAX"] =  idx_gen.QC1MAX
    INPUT_VARIABLE["QC2MIN"] =  idx_gen.QC2MIN
    INPUT_VARIABLE["QC2MAX"] =  idx_gen.QC2MAX
    INPUT_VARIABLE["RAMP_AGC"] =  idx_gen.RAMP_AGC
    INPUT_VARIABLE["RAMP_10"] =  idx_gen.RAMP_10
    INPUT_VARIABLE["RAMP_30"] =  idx_gen.RAMP_30
    INPUT_VARIABLE["RAMP_Q"] =  idx_gen.RAMP_Q
    INPUT_VARIABLE["APF"] =  idx_gen.APF
    
    INPUT_VARIABLE_KEYS = list(INPUT_VARIABLE.keys())
    
    def __init__(self, DUKESVersion = 2019, numOfBus = 10, Location = 'http://dbpedia.org/resource/United_Kingdom'):
        self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EGen\\"
        self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EGen\\Sleepycat_EBus"
        
        self.DUKESVersion = DUKESVersion
        self.numOfBus = numOfBus
        self.location = Location
        
        # Model input
        self.INPUT_VARIABLE_KEYS[0] = None # BUS
        self.INPUT_VARIABLE_KEYS[1] = None # PG_INPUT
        self.INPUT_VARIABLE_KEYS[2] = 0 # QG_INPUT
        self.INPUT_VARIABLE_KEYS[3] = None # QMAX
        self.INPUT_VARIABLE_KEYS[4] = None # QMIN
        self.INPUT_VARIABLE_KEYS[5] = 1 # VG
        self.INPUT_VARIABLE_KEYS[6] = 100 # MBASE
        self.INPUT_VARIABLE_KEYS[7] = 1 # STATUS
        self.INPUT_VARIABLE_KEYS[8] = None # PMAX
        self.INPUT_VARIABLE_KEYS[9] = None # PMIN
        self.INPUT_VARIABLE_KEYS[10] = 0 # PC1
        self.INPUT_VARIABLE_KEYS[11] = 0 # PC2
        self.INPUT_VARIABLE_KEYS[12] = 0 # QC1MIN
        self.INPUT_VARIABLE_KEYS[13] = 0 # QC1MAX
        self.INPUT_VARIABLE_KEYS[14] = 0 # QC2MIN
        self.INPUT_VARIABLE_KEYS[15] = 0 # QC2MAX
        self.INPUT_VARIABLE_KEYS[16] = 0 # RAMP_AGC
        self.INPUT_VARIABLE_KEYS[17] = 0 # RAMP_10
        self.INPUT_VARIABLE_KEYS[18] = 0 # RAMP_30
        self.INPUT_VARIABLE_KEYS[19] = 0 # RAMP_Q
        self.INPUT_VARIABLE_KEYS[20] = 0 # APF
        
        # Model output
        self.PG_OUTPUT = None
        self.QG_OUTPUT = None
        
    def __dir__(self):
        return {"INPUT": self.INPUT_VARIABLE, "OUTPUT": self.OUTPUT_VARIABLE}
    
    
class UKEGenModel_CostFunc(UKEGenModel): 
    
    """Cost function parametor keys"""
    CostFuncFormatKey = "Format_CostEq_"
    StartupCostKey = "StartupCost_"
    ShutdownCostKey = "ShutdownCost_"
    genCostnKey = "genCostn_"
    genCost_aKey = "genCostcn-2_a_"
    genCost_bKey = "genCostcn-2_b_"
    genCost_cKey = "genCostcn-2_c_"
    
    """Initialise the cost function"""
    def __init__(self, DUKESVersion = 2019, CarbonTax = 16, piecewiseOrPolynomial = 2, pointsOfPiecewiseOrcostFuncOrder = 3, Location = 'http://dbpedia.org/resource/United_Kingdom'): # 2019 base world UK carbon tax is £16/tCO2 eq.
            self.DUKESVersion = DUKESVersion
            self.location = Location
            self.MODEL = piecewiseOrPolynomial # 1: piecewise linear;  2: polynomial
            self.STARTUP = 0
            self.SHUTDOWN = 0
            self.NCOST = pointsOfPiecewiseOrcostFuncOrder
            self.CarbonTax = CarbonTax




