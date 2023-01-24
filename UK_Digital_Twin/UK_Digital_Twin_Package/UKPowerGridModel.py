##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 17 March 2022        #
##########################################

"""This module declare the properties of generating UK power grid model A-boxes"""
import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from pypower import idx_bus
from pypower import idx_brch
from pypower import idx_gen, idx_cost
import collections
from pathlib import Path

valueKey = "ScalarValue_"
ModelVariableSpecificationKey = "ModelVariableSpecification_"

powerSystemModelKey = "PowerSystemModel_"

SLASH = '/'

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Default remote endpoint"""
endpoint = EndPointConfigAndBlazegraphRepoLabel.UKPowerGridModelKG

"""The namespace of ontopoesys"""
ontopowsys_namespace = dt.baseURL + SLASH + t_box.ontopowsysName + SLASH

class UKEbusModel:
    
    """Data path"""
    DataPath = str(Path(__file__).resolve().parent.parent) + "\Data files\PowerGridModelInitialisation\\"
    
    """EBus Node keys"""
    ModelEBusKey = "ElectricalBusModel_"
    EBusKey = "EBus-"
    
    """Model variable keys"""
    BUSNUMKey = "BusNumber_"
    BUSTYPEKey = "BusType_"
    PD_INPUTKey = "PdBus_"
    GD_INPUTKey = "GdBus_"
    GSKey = "Gs_"
    BSKey = "Bs_"
    AREAKey = "Area_"
    VM_INPUTKey = "Vm_"
    VA_INPUTKey = "Va_"
    BASEKVKey = "baseKV_"
    ZONEKey = "Zone_"
    VMAXKey = "VmMax_"
    VMINKey = "VmMin_"
    
    """Model output keys"""
    PD_OUTPUTKey = "PdBus_"
    GD_OUTPUTKey = "GdBus_"
    VM_OUTPUTKey = "Vm_"
    VA_OUTPUTKey = "Va_"
    PG_OUTPUTKey = "PdGen_"
    GG_OUTPUTKey = "GdGen_"
    
    
    """Data file header"""
    headerBusModel = ["Bus", "Type", "Pd", "Gd", "Gs", "Bs", "area", "Vm", "Va", "basekV", "zone", "Vmax", "Vmin", "x-axis", "y-axis\n"]
    
    # Map with the bus indices of the pypower package
    
    """The dictionary index key for PF analysis """
   # INPUT_VARIABLE = ["BUS", "TYPE", "PD_INPUT", "GD_INPUT", "GS", "BS", "AREA", "VM_INPUT", "VA_INPUT", "BASEKV", "ZONE", "VMAX", "VMIN"]
   # OUTPUT_VARIABLE = ["VM_OUTPUT", "VA_OUTPUT", "P_GEN", "G_GEN", "PD_OUTPUT", "GD_OUTPUT"]
    
    ##--Input keys--##
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
    
    startingIndexOfOutput = len(INPUT_VARIABLE)
    OUTPUT_VARIABLE = collections.OrderedDict()
    OUTPUT_VARIABLE["VM_OUTPUT"] = 0
    OUTPUT_VARIABLE["VA_OUTPUT"] = 1
    OUTPUT_VARIABLE["P_GEN"] = 2
    OUTPUT_VARIABLE["G_GEN"] = 3
    OUTPUT_VARIABLE["PD_OUTPUT"] = 4
    OUTPUT_VARIABLE["GD_OUTPUT"] = 5
    
    OUTPUT_VARIABLE_KEYS = list(OUTPUT_VARIABLE.keys())
    
    # {"BUS":"BUS_I", "TYPE": "BUS_TYPE", "PD_INPUT": "PD", "GD_INPUT": "QD", "GS": "GS", "BS": "BS", "AREA": "BUS_AREA", "VM_INPUT": "VM", "VA_INPUT": "VA", "BASEKV": "BASE_KV", "ZONE": "ZONE", "VMAX":"VMAX", "VMIN": "VMIN" }
    
    def __init__(self, numOfBus:int, BusNodeIRI:str):
        self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EBus\\"
        self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EBus\\Sleepycat_EBus"
        self.BusModelInitialisation = UKEbusModel.DataPath + str(numOfBus) + '_bus\\BusModelInitialisation.csv'        
        self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EBus\\Sleepycat_EBus"
        # self.DUKESVersion = DUKESVersion
        # self.numOfBus = numOfBus
        # self.location = Location
        
        # Model input
        self.BUS = None # BUS
        self.TYPE = 1 # TYPE
        self.PD_INPUT = None # PD_INPUT
        self.GD_INPUT = 0 # GD_INPUT
        self.GS = 0 # GS
        self.BS = 0 # BS
        self.AREA = 1 # AREA 
        self.VM_INPUT = 1 # VM_INPUT
        self.VA_INPUT = 0 # VA_INPUT            
        self.BASEKV = 400 # BASEKV
        self.ZONE = 1 # ZONE
        self.VMAX = 1.05 # VMAX
        self.VMIN = 0.95 # VMIN
        
        # Model output
        self.PD_OUTPUT = None
        self.GD_OUTPUT = None
        self.VM_OUTPUT = None
        self.VA_OUTPUT = None  
        self.P_GEN = None
        self.G_GEN = None
        
        # Bus node IRI
        self.BusNodeIRI = BusNodeIRI
        
    def __dir__(self):
        return self.INPUT_VARIABLE_KEYS + self.OUTPUT_VARIABLE_KEYS
        #return {"INPUT": self.INPUT_VARIABLE, "OUTPUT": self.OUTPUT_VARIABLE}
         

class UKElineModel:
    
    """Data path"""
    DataPath = str(Path(__file__).resolve().parent.parent) + "\Data files\PowerGridModelInitialisation\\"
    
    """ELine Node keys"""
    ELineKey = "ELine-"
    ModelELineKey = "ElectricalLine_"
    
    """Model input variable keys"""
    FROMBUSKey = "BusFrom_"
    TOBUSKey = "BusTo_"
    R_Key = "R_"
    X_Key = "X_"
    B_Key = "B_"
    RateAKey = "RateA_"
    RateBKey = "RateB_"
    RateCKey = "RateC_"
    RATIOKey = "RatioCoefficient_"
    ANGLEKey = "Angle_"
    STATUSKey = "BranchStatus_"
    ANGMINKey = "AngleMin_"
    ANGMAXKey = "AngleMax_"

    """Model output keys"""
    FROMBUSINJECTION_P_Key = "PdBus_"
    FROMBUSINJECTION_Q_Key = "GdBus_"
    TOBUSINJECTION_P_Key = "Vm_"
    TOBUSINJECTION_Q_Key = "Va_"
    LOSS_P_Key = "PLoss_"
    LOSS_Q_Key = "QLoss_"
    
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
    
    startingIndexOfOutput = len(INPUT_VARIABLE)
    OUTPUT_VARIABLE = collections.OrderedDict()
    OUTPUT_VARIABLE["FROMBUSINJECTION_P"] = 0
    OUTPUT_VARIABLE["FROMBUSINJECTION_Q"] = 1
    OUTPUT_VARIABLE["TOBUSINJECTION_P"] = 2
    OUTPUT_VARIABLE["TOBUSINJECTION_Q"] = 3
    OUTPUT_VARIABLE["LOSS_P"] = 4
    OUTPUT_VARIABLE["LOSS_Q"] = 5
    
    OUTPUT_VARIABLE_KEYS = list(OUTPUT_VARIABLE.keys())
    
    def __init__(self, numOfBus:int, BranchNodeIRI:str, initialiserMethod = 'defaultBranchInitialiser'):
        
        self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\ELine\\"
        self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\ELine\\Sleepycat_EBus"
        self.BranchProperty =  UKElineModel.DataPath + str(numOfBus) + '_bus\\branch_properties.csv' # the branch prop should be calculated from the raw data
        self.BranchModelInitialisation = UKElineModel.DataPath + str(numOfBus) + '_bus\\BranchModelInitialisation.csv'             
        self.BranchInfo = str(Path(__file__).resolve().parent.parent) + "\Data files\PowerGridTopology\\" + str(numOfBus) + '_bus\\branch_topological_info.csv'
        if initialiserMethod == 'defaultBranchInitialiser':
            self.headerBranchProperty = ["voltage_level_kV", "R_MVA/km", "X_MVA/km", "B_MVA/km", "MVA\n"]
        elif initialiserMethod == 'preSpecifiedBranchInitialiser':
            self.headerBranchProperty = ["Bus1", "Bus2", "R", "X", "B", "RateA", "RateB", "RateC", "ratio",	"angle", "status", "angmin", "angmax\n"]
        else:
            self.headerBranchProperty = []
        
     
        self.numOfBus = numOfBus

        
        ## Attributes ##
        # model input
        self.FROMBUS = None # FROMBUS
        self.TOBUS = None # TOBUS
        self.R = None # R
        self.X = None # X
        self.B = None # B
        self.RateA = None # RateA
        self.RateB = 0 # RateB
        self.RateC = 0 # RateC
        self.RATIO = 1 # RATIO
        self.ANGLE = 0 # ANGLE
        self.STATUS = 1 # STATUS
        self.ANGMIN = -360 # ANGMIN
        self.ANGMAX = 360 # ANGMAX
        
        # The PF analysis output
        self.FROMBUSINJECTION_P = None
        self.FROMBUSINJECTION_Q = None
        self.TOBUSINJECTION_P = None
        self.TOBUSINJECTION_Q = None
        self.LOSS_P = None
        self.LOSS_Q = None

        # Eline node IRI
        self.BranchNodeIRI = BranchNodeIRI
    
    def __dir__(self):
        return self.INPUT_VARIABLE_KEYS + self.OUTPUT_VARIABLE_KEYS
        #return {"INPUT": self.INPUT_VARIABLE, "OUTPUT": self.OUTPUT_VARIABLE}       

class UKEGenModel:
    
    """EGen Node keys"""
    EGenKey = "EGen-"
    EGenRetrofitKey = "EGenRetrofit-"
    EGenBackUpKey = "EGenBackUp-"

    ModelEGenKey = "ElectricalGeneratorModel_"
    
    """Model variable keys"""
    BUSNUMKey = "BusNumber_"
    PG_INPUTKey = "Pg_"
    QG_INPUTKey = "Qg_"
   
    QMAXKey = "QMax_"
    QMINKey = "QMin_"
    VGKey = "Vg_"
    MBASEKey = "mBase_"
    STATUSKey ="Status_"
    PMAXKey = "PMax_"
    PMINKey = "PMin_"
    PC1Key = "Pc1_"
    PC2Key = "Pc2_"
    QC1MINKey = "QC1Min_"
    QC2MINKey = "QC2Min_"
    QC1MAXKey = "QC1Max_"
    QC2MAXKey = "QC2Max_"
    RAMP_AGCKey = "Rampagc_"
    RAMP_10Key = "Ramp10_"
    RAMP_30Key = "Ramp30_"
    RAMP_QKey = "Rampq_"
    APFKey = "APF_"
    
    PG_OUTPUTKey = "Pg_"
    QG_OUTPUTKey = "Qg_"


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
    
    startingIndexOfOutput = len(INPUT_VARIABLE)
    OUTPUT_VARIABLE = collections.OrderedDict()
    OUTPUT_VARIABLE["PG_OUTPUT"] = 0
    OUTPUT_VARIABLE["QG_OUTPUT"] = 1
   
    OUTPUT_VARIABLE_KEYS = list(OUTPUT_VARIABLE.keys())
    
    def __init__(self, numOfBus:int, generatorNodeIRI:str, fueltype:str, latlon:list, capacity:float, toBeRetrofittedGeneratorNodeIRI:str = None, status:str = 'Extant', smallAreaCode = None, RegionLACode = None):
        self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EGen\\"
        self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\EGen\\Sleepycat_EBus"
        
        self.numOfBus = numOfBus
        self.status = status
        self.smallAreaCode = smallAreaCode ## same LA code as the comsumption area code
        self.RegionLACode = RegionLACode
        # Model input
        self.BUS = None # BUS
        self.PG_INPUT = None # PG_INPUT
        self.QG_INPUT = 0 # QG_INPUT
        self.QMAX = None # QMAX
        self.QMIN = None # QMIN
        self.VG = 1 # VG
        self.MBASE = 100 # MBASE
        self.STATUS = 1 # STATUS
        self.PMAX = None # PMAX
        self.PMIN = None # PMIN
        self.PC1 = 0 # PC1
        self.PC2 = 0 # PC2
        self.QC1MIN = 0 # QC1MIN
        self.QC1MAX = 0 # QC1MAX
        self.QC2MIN = 0 # QC2MIN
        self.QC2MAX = 0 # QC2MAX
        self.RAMP_AGC = 0 # RAMP_AGC
        self.RAMP_10 = 0 # RAMP_10
        self.RAMP_30 = 0 # RAMP_30
        self.RAMP_Q = 0 # RAMP_Q
        self.APF = 0 # APF

        # Model output
        self.PG_OUTPUT = None
        self.QG_OUTPUT = None

        # Generator Node IRI
        self.generatorNodeIRI = generatorNodeIRI ## IRI represents the existing generator or the newly created IRI represents the SMR 
        self.toBeRetrofittedGeneratorNodeIRI = toBeRetrofittedGeneratorNodeIRI ## the old generator that will be replaced with the new generator
        
        ## Locaion 
        self.latlon = latlon

        ## original capacity
        self.capacity = float(capacity)

        ## Fuel type
        if "#" in fueltype:
            fueltype = fueltype.split('#')[1]
        elif "/" in fueltype:
            fueltype = fueltype.split('/')[-1]
        self.fueltype = fueltype
        
    def __dir__(self):
        return self.INPUT_VARIABLE_KEYS + self.OUTPUT_VARIABLE_KEYS
        #return {"INPUT": self.INPUT_VARIABLE, "OUTPUT": self.OUTPUT_VARIABLE}
    
    
class UKEGenModel_CostFunc(UKEGenModel):     
    """Cost function parametor keys"""
    CostFuncFormatKey = "CostModel_"
    StartupCostKey = "StartCost_"
    ShutdownCostKey = "StopCost_"
    genCostnKey = "genCostn_"

    genCost_bKey = "ZeroOrderCoefficient_" 
    genCost_aKey = "FirstOrderCoefficient_"
    # genCost_cKey = "SecondOrderCoefficient_"

    """Mapping the index key from PyPower"""
    INPUT_VARIABLE = collections.OrderedDict()
    INPUT_VARIABLE["MODEL"] =  idx_cost.MODEL 
    INPUT_VARIABLE["STARTUP"] =  idx_cost.STARTUP
    INPUT_VARIABLE["SHUTDOWN"] =  idx_cost.SHUTDOWN 
    INPUT_VARIABLE["NCOST"] =  idx_cost.NCOST
    INPUT_VARIABLE["COST"] =  idx_cost.COST  
    
    INPUT_VARIABLE_KEYS = list(INPUT_VARIABLE.keys())

    """Initialise the cost function"""
    def __init__(self, numOfBus:int, generatorNodeIRI:str, CO2EmissionFactor:float, fueltype:str, latlon:list, capacity:float, toBeRetrofittedGeneratorNodeIRI:str = None, status:str = 'Extant', CarbonTax = 18, piecewiseOrPolynomial = 2, pointsOfPiecewiseOrcostFuncOrder = 2, smallAreaCode = None, RegionLACode = None): # 2020/2021 base world UK carbon tax is £18/tCO2 eq.               
            super().__init__(numOfBus, generatorNodeIRI, fueltype, latlon, capacity, toBeRetrofittedGeneratorNodeIRI, status, smallAreaCode, RegionLACode) ## enforce to inherite the initialiser from the father class
            self.MODEL = piecewiseOrPolynomial # 1: piecewise linear; 2: polynomial
            self.STARTUP = 0
            self.SHUTDOWN = 0
            self.NCOST = pointsOfPiecewiseOrcostFuncOrder
            self.COST = []
            self.CarbonTax = CarbonTax
            self.CO2EmissionFactor = CO2EmissionFactor
            