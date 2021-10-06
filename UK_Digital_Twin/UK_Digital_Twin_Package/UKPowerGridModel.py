##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 June 2021         #
##########################################

"""This module declare the properties of generating UK power grid model A-boxes"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel

valueKey = "value_"

"""Default remote endpoint"""
endpoint = EndPointConfigAndBlazegraphRepoLabel.UKPowerGridModelKG

class UKEbusModel:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\10_bus\\EBus\\"
    SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\10_bus\\EBus\\Sleepycat_EBus"
    
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
    
    def __init__(self, DUKESVersion = 2019, numOfBus = 10):
        self.DUKESVersion = DUKESVersion
        self.numOfBus = numOfBus
        self.location = 'http://dbpedia.org/resource/United_Kingdom'
        self.BUS = None
        self.TYPE = 1
        self.PD_INPUT = None
        self.GD_INPUT = 0
        self.PD_OUTPUT = None
        self.GD_OUTPUT = None
        self.GS = 0
        self.BS = 0
        self.AREA = 1       
        self.VM_INPUT = 1
        self.VA_INPUT = 0
        self.VM_OUTPUT = None
        self.VA_OUTPUT = None        
        self.BASEKV = 400
        self.ZONE = 1
        self.VMAX = 1.05
        self.VMIN = 0.95
        self.PDGEN = None
        self.GDGEN = None
        

class UKElineModel:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\10_bus\\ELine\\"
    SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\10_bus\\ELine\\Sleepycat_ELine"
     
    """ELine Node keys"""
    ELineKey = "ELine-"
    ModelELineKey = "Model_ELine-"
    ShapeKey = "Shape_"
    LengthKey = "Length_"
    OHL400kVKey = "OHL_400kV_" # Over head line (OHL) with 400kV level
    OHL275kVKey = "OHL_275kV_" # Over head line (OHL) with 275kV level
    
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
    
    def __init__(self, DUKESVersion = 2019, numOfBus = 10):
        self.DUKESVersion = DUKESVersion
        self.numOfBus = numOfBus
        self.location = 'http://dbpedia.org/resource/United_Kingdom'
        self.FROMBUS = None
        self.TOBUS = None
        self.R = None
        self.X = None
        self.B = None
        self.RateA = None
        self.RateB = 0
        self.RateC = 0
        self.RATIO = 1
        self.ANGLE = 0
        self.STATUS = 1
        self.ANGMIN = -360
        self.ANGMAX = 360

        

class UKEGenModel:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\10_bus\\EGen\\"
    SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\10_bus\\EGen\\Sleepycat_EGen"
    
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
    
    
    def __init__(self, DUKESVersion = 2019, numOfBus = 10):
        self.DUKESVersion = DUKESVersion
        self.numOfBus = numOfBus
        self.location = 'http://dbpedia.org/resource/United_Kingdom'
        self.BUS = None
        self.PG_INPUT = None
        self.QG_INPUT = 0
        self.PG_OUTPUT = None
        self.QG_OUTPUT = None
        self.QMAX = None
        self.QMIN = None
        self.VG = 1
        self.MBASE = 100
        self.STATUS = 1
        self.PMAX = None
        self.PMIN = None
        self.PC1 = 0
        self.PC2 = 0
        self.QC1MIN = 0
        self.QC1MAX = 0
        self.QC2MIN = 0
        self.QC2MAX = 0
        self.RAMP_AGC = 0
        self.RAMP_10 = 0
        self.RAMP_30 = 0
        self.RAMP_Q = 0
        self.APF = 0
    
    
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
    def __init__(self, DUKESVersion = 2019, CarbonTax = 16): # 2019 base world UK carbon tax is £16/tCO2 eq.
            self.DUKESVersion = DUKESVersion
            self.location = 'http://dbpedia.org/resource/United_Kingdom'
            self.MODEL = 2
            self.STARTUP = 0
            self.SHUTDOWN = 0
            self.NCOST = 3
            self.CarbonTax = CarbonTax

# if __name__ == '__main__':    
#     test = UKEGenModel(2019)
#     print(test.EGenKey)    
#     print('Terminated')



