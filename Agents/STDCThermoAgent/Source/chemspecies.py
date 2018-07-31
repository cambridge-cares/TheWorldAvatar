import ElementsData as eld
import params as p
import numpy as np
import thermochemistry as sthd
import utilities as utl
#======================================================
#                 Chemical species class
#======================================================
# default temperature ranges
ToutCsv = [298,300,400,500,600,700,800,900,1000,1200,1500,1700,2000,2500,3000]
TfitNasa = [298.15,1000,2500]

class ChemSpecies:
    #--------------------------------------------------
    #            Constructor
    #--------------------------------------------------
    def __init__(self,aName="",aFormula="",aComp=[],aMolWt=0.0,aElMolWt=[],aVibFreq=[],aVibScale=1.0,aSymNr=1.0,aGeom=[],
                aGeomType=2,aImom=[],aElecEn=0.0,aZPE=0.0,aSpinMult=1.0,aElecLvL=[[1.0,0.0]],aLowNasa=[],
                aHighNasa=[],aTrangeNasa=[],aEnthRefSource='#None',aEnthRefTemp=298.15,aEnthRef=0.0,aFitLowNasa=[],
                aFitHighNasa=[],aFitTrangeNasa=TfitNasa,aToutCsv=ToutCsv,aMetaData={}):
        #   properties set based on constructor arguments
        #--------------------------------------------------
        # attribute name                               SI units
        self.Name = aName                        #     [-]
        self.Formula = aFormula                  #     [-]
        self.Composition = aComp                 #     [-]
        self.MolWt = aMolWt                      #     [kg]
        self.VibFreq = aVibFreq                  #     [1/s (Hz)]
        self.VibFreqScaleFactor = aVibScale      #     [-]
        self.SymNr = aSymNr                      #     [-]
        self.Geometry = aGeom                    #     [-, 3xm]
        # GeomType: 0 -atom, 1- linear, 2- nonlinear
        self.GeomType = aGeomType                #     [-]
        self.InertiaMom = aImom                  #     [kg*m^2]
        self.ElecEn = aElecEn                    #     [J] where Ha = Hartree
        self.ZPE = aZPE                          #     [J]
        self.SpinMult = aSpinMult                #     [-]
        self.ElecLvL = aElecLvL                  #     [-,J]
        self.LowNasa = aLowNasa                  #     [7x-]
        self.HighNasa = aHighNasa                #     [7x-]
        self.TrangeNasa = aTrangeNasa            #     [K,K,K]
        self.EnthRefSource = aEnthRefSource      #     [-]
        self.EnthRef = aEnthRef                  #     [J/mol]
        self.EnthRefTemp = aEnthRefTemp          #     [K]
        self.FitLowNasa = aFitLowNasa
        self.FitHighNasa = aFitHighNasa
        self.FitTrangeNasa = aFitTrangeNasa
        self.ElMolWt = aElMolWt                  #     [kg]
        self.ToutCsv = aToutCsv                  #     [n2 x K]
        self.MetaData = aMetaData                #     [-]
        #   properties set based on other properties
        #--------------------------------------------------
        self.RotConst = []                       #     [1/m]
        self.VibTemp = []                        #     [K]
        self.RotTemp = []                        #     [K]
        self.XYZ = []                            #     [m]
        self.EnthRefCorr = 0.0                   #     [J/mol]
        self = self.setDerivedProperties()

    #======================================================
    #            Methods
    #======================================================

    #---------------------------------
    # Methods to set/get other fields
    #---------------------------------

    # set dependent chemspecies fields
    #---------------------------------
    def setDerivedProperties(self):
        # get molecular mass and elements mass vector
        if not self.ElMolWt:
            self.ElMolWt = self.getElMolWt()
        if self.MolWt<=0:
            self.MolWt = self.getMolWt()
        if len(self.VibFreq)>0:
            if self.VibFreqScaleFactor != 1.0:
                self.VibFreq = [x*self.VibFreqScaleFactor for x in self.VibFreq]
            self.VibTemp = self.getVibTemp()
            if self.ZPE == 0.0:
                self.ZPE = self.getZPE()  # J
        if len(self.Geometry)>0:
            self.XYZ = self.getXYZ()
            if len(self.InertiaMom)==0:
                self.InertiaMom = self.getSpMomentsOfInertia()
        if len(self.InertiaMom)!=0:
            self.RotConst = self.getRotConst()
            self.RotTemp = self.getRotTemp()
        if self.EnthRefSource.upper()!='#NONE':
            if self.EnthRefSource.upper()=='#NASA':
                self.EnthRef = self.getSpEnthalpyNASA(self.EnthRefTemp)
                self.EnthRefCorr = self.getSpHcorrSTHD()
            else:
                self.EnthRefCorr = self.getSpHcorrSTHD()
        return self     
        
    # Get mass vector of species elements
    #---------------------------------
    def getElMolWt(self):
        m = []
        u_fact = utl.convertMassUnitsToSI('AMU') # amu => kg
        # get the vector order from the geometry
        if len(self.Geometry)!= 0:
            Elements = [row[0] for row in self.Geometry]
            for el in Elements:
                mi = eld.getCompMolWt([el, 1.0]) # these are in amu                
                m.append(mi*u_fact)
        # get the vector order from the composition
        elif len(self.Composition)!= 0:
            for i in range(0,len(self.Composition),2):
                el = eld.getElData(self.Composition[i])         
                m.append(el[1]*u_fact)
        return m

    # get molecular mass of the species
    #---------------------------------
    def getMolWt(self):
        MolWt = 0.0
        # read it direclty
        if self.MolWt>0:
            MolWt = self.MolWt
        # calculate it
        elif len(self.ElMolWt)>0:
            MolWt = sum(self.ElMolWt)
        return MolWt

    # Gets XYZ coordinate array
    #---------------------------------
    def getXYZ(self):
        if len(self.Geometry)>0:
            XYZ = np.zeros([len(self.Geometry),len(self.Geometry[0])-1],dtype=float)
            for i,row in enumerate(self.Geometry):
                XYZ[i][0] = row[1]
                XYZ[i][1] = row[2]
                XYZ[i][2] = row[3]
        return XYZ

    # Get moments of Inertia 
    # of a molecule
    #---------------------------------
    def getSpMomentsOfInertia(self):
        InertiaMom= utl.getMomentsOfInertia(self.ElMolWt,self.XYZ,self.GeomType)
        InertiaMom= sorted(InertiaMom,reverse=True)
        if self.GeomType==1:
            InertiaMom= [InertiaMom[0],InertiaMom[1]]
        return InertiaMom

    #  Get rotational constants
    #--------------------------
    def getRotConst(self):
        RotConst = [] # in 1/m
        if len(self.InertiaMom)>0:
            RotConst = getRotConstFromImom(self.InertiaMom)
        return RotConst

    # get rotational temperatures
    #--------------------------
    def getRotTemp(self):
        RotTemp = [] # in K
        u_fact = utl.convertEnergyMoleculeUnitsToSI('1/M') # 1/m => J
        u_fact = u_fact * utl.convertEnergyMoleculeUnitsToSI('K', -1.0) # J=>K
        if len(self.RotConst)>0:
            for B in self.RotConst:
                RotTemp.append(B*u_fact)
        return RotTemp

    # get vibrational temperatures
    #--------------------------
    def getVibTemp(self):
        VibTemp = [] # in K
        u_fact = utl.convertEnergyMoleculeUnitsToSI('1/S') # 1/s => J
        u_fact = u_fact * utl.convertEnergyMoleculeUnitsToSI('K', -1.0) # J=>K
        if len(self.VibFreq)>0: # in 1/s
            for vib in self.VibFreq:
                if vib>0.0:
                    VibTemp.append(vib*u_fact)
        return VibTemp

    # get zero point energy
    #--------------------------
    def getZPE(self):
        ZPE = 0.0
        for thetaTV in self.VibTemp:
            if thetaTV>0.0:
                ZPE = ZPE + thetaTV/2.0
        u_fact = utl.convertEnergyMoleculeUnitsToSI('K') # K=>J/mol
        ZPE = ZPE*u_fact
        return ZPE

    # Partition function of a species
    #--------------------------
    def getSpPartFunc(self,T):
        qt = sthd.getPartFunc(self.MolWt,self.GeomType,
           self.SymNr,self.RotTemp,self.VibTemp,self.ElecLvL,T)
        return q

    # Entropy of a species from stat. thermodyn.
    #--------------------------
    def getSpEntropySTHD(self,T,Sref=0.0):
        S = sthd.getEntropy(self.MolWt,self.GeomType,self.SymNr,
            self.RotTemp,self.VibTemp,self.ElecLvL,T,P=p.Pref)
        S = S + Sref
        return S

    # Heat Capacity Cv of a species from stat. thermodyn.
    #--------------------------
    def getSpHeatCapacityCvSTHD(self,T):
        Cv = sthd.getHeatCapacityCv(self.GeomType,self.VibTemp,self.ElecLvL,T)
        return Cv

    # Heat Capacity Cp of a species from stat. thermodyn.
    #--------------------------
    def getSpHeatCapacityCpSTHD(self,T):
        Cp = sthd.getHeatCapacityCp(self.GeomType,self.VibTemp,self.ElecLvL,T)
        return Cp

    # Enthalpy of a species from stat. thermodyn.
    #--------------------------
    def getSpEnthalpySTHD(self,T):
        H = sthd.getEnthalpy(self.GeomType,self.VibTemp,self.ElecLvL,T)
        H = H + self.EnthRefCorr
        return H

    # Internal Energy of a species from stat. thermodyn.
    #--------------------------
    def getSpInternalEnergySTHD(self,T):
        H = self.getSpEnthalpySTHD(T)
        U = H-p.R*T
        return U

    # Gibbs Energy of a species from stat. thermodyn.
    #--------------------------
    def getSpGibbsEnergySTHD(self,T):
        S = self.getSpEntropySTHD(T)
        H = self.getSpEnthalpySTHD(T)
        G = H-T*S
        return G


    # Entropy of a species from NASA polynomials
    #--------------------------
    def getSpEntropyNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            S = utl.getEntropy(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            S = utl.getEntropy(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return S

    # Internal Energy of a species from NASA polynomials
    #--------------------------
    def getSpInternalEnergyNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            U = utl.getInternalEnergy(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            U = utl.getInternalEnergy(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return U

    # Heat Capacity Cv of a species from NASA polynomials
    #--------------------------
    def getSpHeatCapacityCvNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            Cv = utl.getHeatCapacityCv(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            Cv = utl.getHeatCapacityCv(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return Cv

    # Heat Capacity Cp of a species from NASA polynomials
    #--------------------------
    def getSpHeatCapacityCpNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            Cp = utl.getHeatCapacityCp(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            Cp = utl.getHeatCapacityCp(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return Cp

    # Enthalpy of a species from NASA polynomials
    #--------------------------
    def getSpEnthalpyNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            H = utl.getEnthalpy(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            H = utl.getEnthalpy(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return H

    # Gibbs Energy of a species from NASA polynomials
    #--------------------------
    def getSpGibbsEnergyNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            G = utl.getGibbsEnergy(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            G = utl.getGibbsEnergy(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return G

    # Correction to enthalpy to shift it to its refrence point
    #--------------------------
    def getSpHcorrSTHD(self):
        Hcorr = 0.0
        Href=self.EnthRef
        H1 = sthd.getEnthalpy(self.GeomType,self.VibTemp,self.ElecLvL,self.EnthRefTemp)
        Hcorr = Href-H1
        return Hcorr

#======================================================
#            Utility functions
#======================================================
def getDefaultProps():
    # Returns default properties of a chemical species
    defaultProps = {}
    defaultProps['Name']=""
    defaultProps['Formula']=""
    defaultProps['Comp']=[]
    defaultProps['MolWt']=0.0
    defaultProps['ElMolWt']=[]
    defaultProps['VibFreq']=[]
    defaultProps['VibScale']=1.0
    defaultProps['SymNr']=1.0
    defaultProps['Geom']=[]
    defaultProps['GeomType']=2
    defaultProps['Imom']=[]
    defaultProps['ElecEn']=0.0
    defaultProps['ZPE']=0.0
    defaultProps['SpinMult']=1.0
    defaultProps['ElecLvL']=[[1.0,0.0]]
    defaultProps['LowNasa']=[]
    defaultProps['HighNasa']=[]
    defaultProps['TrangeNasa']=[]
    defaultProps['EnthRefSource']='#None'
    defaultProps['EnthRefTemp']=298.15
    defaultProps['EnthRef']=0.0
    defaultProps['FitLowNasa']=[]
    defaultProps['FitHighNasa']=[]
    defaultProps['FitTrangeNasa']=TfitNasa
    defaultProps['ToutCsv']=ToutCsv
    defaultProps['MetaData']={}
    return defaultProps

def CreateChemSpecFromDict(dict):
    # Get defaults
    def_dict = getDefaultProps()

    # Update dict with any missing keys
    for key, value in def_dict.items():
        if key not in dict:
            dict[key] = value

    # Create species
    rSpec = ChemSpecies(aName=dict['Name'],aFormula=dict['Formula'],aComp=dict['Comp'],
            aMolWt=dict['MolWt'],aElMolWt=dict['ElMolWt'],aVibFreq=dict['VibFreq'],
            aVibScale=dict['VibScale'],aSymNr=dict['SymNr'],aGeom=dict['Geom'],
            aGeomType=dict['GeomType'],aImom=dict['Imom'],aElecEn=dict['ElecEn'],
            aZPE=dict['ZPE'],aSpinMult=dict['SpinMult'],aElecLvL=dict['ElecLvL'],
            aLowNasa=dict['LowNasa'],aHighNasa=dict['HighNasa'],aTrangeNasa=dict['TrangeNasa'],
            aEnthRefSource=dict['EnthRefSource'],aEnthRefTemp=dict['EnthRefTemp'],
            aEnthRef=dict['EnthRef'],aFitLowNasa=dict['FitLowNasa'],aFitHighNasa=dict['FitHighNasa'],
            aFitTrangeNasa=dict['FitTrangeNasa'],aToutCsv=dict['ToutCsv'],aMetaData=dict['MetaData'])
    return rSpec

def getRotConstFromImom(aImom):
    RotConst = [] # B in 1/m
    if len(aImom)>0:
        for I in aImom:
            if I>0.0: # I in kg*m^2
                RotConst.append(p.BI_pref/I)
    return RotConst

def getImomFromRotConstant(aRotConst):
    InertiaMom = [] # I in kg*m^2
    if len(aRotConst)>0:
        for B in aRotConst:
            if B>0.0: # B in 1/m
                InertiaMom.append(p.BI_pref/B)
    return InertiaMom

def GetThermoData(T,Spec,unit=1.0,inclNasa=False,inclFitNasa=True):
    ncols = 6
    if unit==1:
        unitlabel1 = '[J/mol]'
        unitlabel2 = '[J/mol/K]'
    else:
        unitlabel1 = ''
        unitlabel2 = ''
    DataHeaders = ['T [K]','S sthd '+unitlabel2, 'H sthd '+unitlabel1, 'Cp sthd '+unitlabel2, 'Cv sthd '+unitlabel2, 'U sthd '+unitlabel1, 'G sthd '+unitlabel1]
    if inclNasa:
        ncols = ncols + 6
        DataHeaders =  DataHeaders + ['S nasa '+unitlabel2, 'H nasa '+unitlabel1, 'Cp nasa '+unitlabel2, 'Cv nasa '+unitlabel2, 'U nasa '+unitlabel1, 'G nasa '+unitlabel1]
    if inclFitNasa:
        ncols = ncols + 6
        DataHeaders =  DataHeaders + ['S fitnasa '+unitlabel2, 'H fitnasa '+unitlabel1, 'Cp fitnasa '+unitlabel2, 'Cv fitnasa '+unitlabel2, 'U fitnasa '+unitlabel1, 'G fitnasa '+unitlabel1]
    
    Data = np.zeros([len(T),ncols+1])
    for i,Ti in enumerate(T):
        nc = 6
        Data[i][0] = Ti
        Data[i][1] = Spec.getSpEntropySTHD(Ti)*unit
        Data[i][2] = Spec.getSpEnthalpySTHD(Ti)*unit
        Data[i][3] = Spec.getSpHeatCapacityCpSTHD(Ti)*unit
        Data[i][4] = Spec.getSpHeatCapacityCvSTHD(Ti)*unit
        Data[i][5] = Spec.getSpInternalEnergySTHD(Ti)*unit
        Data[i][6] = Spec.getSpGibbsEnergySTHD(Ti)*unit

        if inclNasa:
            Data[i][nc+1] = Spec.getSpEntropyNASA(Ti)*unit
            Data[i][nc+2] = Spec.getSpEnthalpyNASA(Ti)*unit
            Data[i][nc+3]  = Spec.getSpHeatCapacityCpNASA(Ti)*unit
            Data[i][nc+4]  = Spec.getSpHeatCapacityCvNASA(Ti)*unit
            Data[i][nc+5]  = Spec.getSpInternalEnergyNASA(Ti)*unit
            Data[i][nc+6]  = Spec.getSpGibbsEnergyNASA(Ti)*unit
            nc = nc + 6

        if inclFitNasa:
            Data[i][nc+1] = Spec.getSpEntropyNASA(Ti,inclFitNasa)*unit
            Data[i][nc+2] = Spec.getSpEnthalpyNASA(Ti,inclFitNasa)*unit
            Data[i][nc+3] = Spec.getSpHeatCapacityCpNASA(Ti,inclFitNasa)*unit
            Data[i][nc+4] = Spec.getSpHeatCapacityCvNASA(Ti,inclFitNasa)*unit
            Data[i][nc+5] = Spec.getSpInternalEnergyNASA(Ti,inclFitNasa)*unit
            Data[i][nc+6] = Spec.getSpGibbsEnergyNASA(Ti,inclFitNasa)*unit
    return Data,DataHeaders

def getSpByNameFromList(SpList,aName):
    rsp = SpList[0]
    for sp in SpList:
        if sp.Name == aName:
            rsp = sp
            break
    return rsp