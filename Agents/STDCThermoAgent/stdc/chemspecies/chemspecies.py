import numpy as np
import stdc.utils.utils as utl
import stdc.utils.params as p
from stdc.utils.geomtypes import GeomTypes
import stdc.errorhandling.exceptions as stdcerr
import stdc.thermocalculator.sthdcalculator as sthd
import stdc.utils.nasafitter as nasafitter
import stdc.utils.nasablockwriter as nasawriter
import textwrap

#======================================================
#                 Chemical species class
#======================================================
# default temperature ranges
defaultTrange = "298.15,300,400,500,600,700,800,900,1000,1200,1500,1700,2000,2500,3000"
defaultNasaFitTemps = "298.15,1000,3000"
warnings = True
class ChemSpecies:
    def __init__(self,
                 chem_formula,
                 spin_mult,
                 elec_energy,
                 zpe_included,
                 mol_weight,
                 sym_number,
                 rot_constants="",
                 frequencies="",
                 freq_scale_factor="1.0",
                 elec_levels="",
                 temperature="298.15",
                 pressure="1e5",
                 enthalpy_ref_temp="298.15",
                 enthalpy_ref="0.0",
                 temperature_range=defaultTrange,
                 fit_nasa_temperatures=defaultNasaFitTemps,
                 fit_nasa=False):
        #   properties set based on constructor arguments
        #--------------------------------------------------
        # attribute name                               SI units
        self.ChemFormula = chem_formula                                 #        
        self.SpinMult = int(spin_mult)                                  #     [-]
        self.ElecEn = float(elec_energy)                                #     [J]
        self.MolWt = float(mol_weight)                                  #     [kg]
        self.SymNr = int(sym_number)                                    #     [-]
        self.RotConst = _strInputToList(rot_constants,type=float)       #     [1/m]
        self.VibFreq = _strInputToList(frequencies,type=float)          #     [1/s (Hz)]
        self.VibFreqScaleFactor = float(freq_scale_factor)              #     [-]
        self.ElecLvL = _strInputToListOfLists(elec_levels,type=float)   #     [-,J]
        self.EnthRef = float(enthalpy_ref)                              #     [J/mol]
        self.EnthRefTemp = float(enthalpy_ref_temp)                     #     [K]
        #  Reuquested output temp and press + temp range for extra output
        #--------------------------------------------------
        self.RequestedTemp = float(temperature)                         #     [K]
        self.RequestedPressure = float(pressure)                        #     [Pa]
        self.RequestedTrange = _strInputToList(temperature_range,type=float)  #     [n2 x K]
        self.RequestedTPPointData = {}                #
        self.RequestedTrangeData = {}                 #
        #  NASA polynomials fitting - extra output
        #--------------------------------------------------
        self.FitNasa = fit_nasa
        self.LowNasaCoeffs = []                     #     [7x-]
        self.NasaPolynomialsData = {}               #
        self.HighNasaCoeffs = []                    #     [7x-]
        self.NasaComment = 'STHD'                   #
        self.NasaPhase = 'G'                        #
        self.NasaFitTemps = _strInputToList(fit_nasa_temperatures,type=float)  #     [K,K,K]
        #   properties set based on other properties
        #--------------------------------------------------
        self.AtomsCounts = utl.chemFormulaToAtomsCounts(chem_formula)  #     [-]
        self.AtomsNum = sum(self.AtomsCounts.values())                 #     [-]
        self._removeZeroRotConst()
        self._setGeomType()
        self._checkFreqAndRotConstNums()
        self._checkSpinMult()
        self._checkElecLvls()
        self._applyVibFreqScaleFactor()
        self._setVibTemp()                         #     [K]
        self._setRotTemp()                         #     [K]
        if not zpe_included: self._addZPE()

    #======================================================
    #            Methods
    #======================================================

    def getThermoData(self):
        self._getRequestedTPPointData()
        self._getRequestedTrangeData()
        if self.fitNasa: self._getNasaPolynomialsData()

    def getZPE(self):
        ZPE = 0.0
        for thetaTV in self.VibTemp:
            if thetaTV > 0.0:
                ZPE = ZPE + thetaTV/2.0
        u_fact = utl.convertEnergyMoleculeUnitsToSI('K') # K=>J/mol
        ZPE = ZPE*u_fact
        return ZPE

    def getSpPartFunc(self,T):
        q = sthd.getPartFunc(self.MolWt,self.GeomType,
           self.SymNr,self.RotTemp,self.VibTemp,self.ElecLvL,T)
        return q

    def getSpEntropySTHD(self,T,P=p.Pref,Sref=0.0):
        S = sthd.getEntropy(self.MolWt,self.GeomType,self.SymNr,
            self.RotTemp,self.VibTemp,self.ElecLvL,T,P)
        S = S + Sref
        return S

    def getSpHeatCapacityCvSTHD(self,T):
        Cv = sthd.getHeatCapacityCv(self.GeomType,self.VibTemp,self.ElecLvL,T)
        return Cv

    def getSpHeatCapacityCpSTHD(self,T):
        Cp = sthd.getHeatCapacityCp(self.GeomType,self.VibTemp,self.ElecLvL,T)
        return Cp

    def getSpEnthalpySTHD(self,T):
        H = sthd.getEnthalpy(self.GeomType,self.VibTemp,self.ElecLvL,T)
        H = H + self.EnthRefCorr
        return H

    def getSpInternalEnergySTHD(self,T):
        H = self.getSpEnthalpySTHD(T)
        U = H-p.R*T
        return U

    def getSpGibbsEnergySTHD(self,T):
        S = self.getSpEntropySTHD(T)
        H = self.getSpEnthalpySTHD(T)
        G = H-T*S
        return G

    def getSpEntropyNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            S = utl.getEntropy(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            S = utl.getEntropy(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return S

    def getSpInternalEnergyNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            U = utl.getInternalEnergy(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            U = utl.getInternalEnergy(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return U

    def getSpHeatCapacityCvNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            Cv = utl.getHeatCapacityCv(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            Cv = utl.getHeatCapacityCv(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return Cv

    def getSpHeatCapacityCpNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            Cp = utl.getHeatCapacityCp(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            Cp = utl.getHeatCapacityCp(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return Cp

    def getSpEnthalpyNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            H = utl.getEnthalpy(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            H = utl.getEnthalpy(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return H

    def getSpGibbsEnergyNASA(self,T,useFittedNasa=False):
        if useFittedNasa:
            G = utl.getGibbsEnergy(self.FitLowNasa,self.FitHighNasa,self.FitTrangeNasa,T)
        else:
            G = utl.getGibbsEnergy(self.LowNasa,self.HighNasa,self.TrangeNasa,T)
        return G

    def getSpHcorrSTHD(self):
        Hcorr = 0.0
        Href=self.EnthRef
        H1 = sthd.getEnthalpy(self.GeomType,self.VibTemp,self.ElecLvL,self.EnthRefTemp)
        Hcorr = Href-H1
        return Hcorr

    # Methods to set/get/check other fields
    #---------------------------------
    def _removeZeroRotConst(self):
        self.RotConst = [aRC for aRC in self.RotConst if aRC > 0.0]

    def _setGeomType(self):   
        if self.AtomsNum <= 0:
            raise stdcerr.InvalidInput()     
        elif self.AtomsNum == 1:
            self.GeomType = GeomTypes.ATOMIC
        elif self.AtomsNum == 2:
            self.GeomType = GeomTypes.LINEAR
        elif self.AtomsNum > 2:
            if len(self.RotConst) == 2:
                self.GeomType = GeomTypes.LINEAR
            elif len(self.RotConst) == 3:
                self.GeomType = GeomTypes.NONLINEAR
            else:
                raise stdcerr.InvalidInput()

    def _checkFreqAndRotConstNums(self):
        rotConstNum = len(self.RotConst)
        freqNum = len(self.VibFreq)
        if self.GeomType == GeomTypes.ATOMIC:
            if rotConstNum != 0:
                raise stdcerr.InvalidInput(textwrap.dedent(f"""
                        Error: Rotational constants should not be defined for atomic species."""))
            if freqNum != 0:
                raise stdcerr.InvalidInput(textwrap.dedent(f"""
                        Error: Vibrational frequencies should not be defined for atomic species."""))

        elif self.GeomType == GeomTypes.LINEAR:
            requiredFreqNum = int(3*self.AtomsNum-6)
            requiredRotConstNum = 2
            if rotConstNum != requiredRotConstNum:
                raise stdcerr.InvalidInput(textwrap.dedent(f"""
                        Error: Linear species requires exactly {requiredRotConstNum} non-zero rotational constants,
                        but {rotConstNum} were given."""))
            if freqNum != requiredFreqNum:
                raise stdcerr.InvalidInput(textwrap.dedent(f"""
                        Error: Linear species requires exactly {requiredFreqNum} non-zero frequnecies,
                        but {freqNum} were given."""))
        else:
            requiredFreqNum = int(3*self.AtomsNum-5)
            requiredRotConstNum = 3
            if rotConstNum != requiredRotConstNum:
                raise stdcerr.InvalidInput(textwrap.dedent(f"""
                        Error: Non-linear species requires exactly {requiredRotConstNum} non-zero rotational constants,
                        but {rotConstNum} were given"""))
            if freqNum != requiredFreqNum:
                raise stdcerr.InvalidInput(textwrap.dedent(f"""
                        Error: Non-Linear species requires exactly {requiredFreqNum} non-zero frequnecies,
                        but {freqNum} were given."""))

    def _checkSpinMult(self):
        if self.SpinMult < 0:
            raise stdcerr.InvalidInput()

    def _checkElecLvls(self):
        if not self.ElecLvL:
            self.ElecLvL = [[self.SpinMult, 0.0]]

    def _applyVibFreqScaleFactor(self):
        if self.VibFreqScaleFactor < 0.0:
            raise stdcerr.InvalidInput()
        if self.VibFreqScaleFactor != 1.0:
            self.VibFreq = [x*self.VibFreqScaleFactor for x in self.VibFreq]
        
    def _setRotTemp(self):
        RotTemp = [] # in K
        u_fact = utl.convertEnergyMoleculeUnitsToSI('1/M') # 1/m => J
        u_fact = u_fact * utl.convertEnergyMoleculeUnitsToSI('K', -1.0) # J=>K
        for B in self.RotConst:
            if B > 0.0:
                RotTemp.append(B*u_fact)
            else:
                raise stdcerr.InvalidInput()
        self.RotTemp = RotTemp

    def _setVibTemp(self):
        VibTemp = [] # in K
        u_fact = utl.convertEnergyMoleculeUnitsToSI('1/S') # 1/s => J
        u_fact = u_fact * utl.convertEnergyMoleculeUnitsToSI('K', -1.0) # J=>K
        for vib in self.VibFreq:
            if vib > 0.0:
                VibTemp.append(vib*u_fact)
            else:
                raise stdcerr.InvalidInput()
        self.VibTemp= VibTemp

    def _addZPE(self):
        self.ElecEn = self.ElecEn + self.getZPE()

    def _getRequestedTPPointData(self):
        T = self.RequestedTemp
        P = self.RequestedPressure
        ThermoData= self._getSpThermoAtTP(T,P)

        self.RequestedTPPointData = self._thermoOutToFormattedDict(
            T=T,
            P=P,
            H=ThermoData['H'],
            S=ThermoData['S'],
            U=ThermoData['U'],
            G=ThermoData['G'],
            Cp=ThermoData['Cp'],
            Cv=ThermoData['Cv'])

    def _getRequestedTrangeData(self):
        T = self.RequestedTrange
        P = self.RequestedPressure

        ThermoData= self._getSpThermoAtTrangeP(T,P)

        self.RequestedTrangeData = self._thermoOutToFormattedDict(
            T=T,
            P=P,
            H=ThermoData['H'],
            S=ThermoData['S'],
            U=ThermoData['U'],
            G=ThermoData['G'],
            Cp=ThermoData['Cp'],
            Cv=ThermoData['Cv'])
    
    def _getNasaPolynomialsData(self):
        self.NasaPolynomialsData= self._fitNasaPolynomials()

    def _fitNasaPolynomials(self):    
        # By default the Trange is subdivided into 20 temperatures
        nT = 20

        P = self.RequestedPressure
        Tlow = self.NasaFitTemps[0]
        Tmid = self.NasaFitTemps[1]
        Thigh = self.NasaFitTemps[2]
        
        TrangeLow = np.linspace(Tlow,Tmid,nT)
        TrangeHigh = np.linspace(Tmid,Thigh,nT)

        ThermoDataLow = self._getSpThermoAtTrangeP(TrangeLow,P)
        ThermoDataHigh = self._getSpThermoAtTrangeP(TrangeHigh,P)

        NasaLowTCoeffs = nasafitter.fitNASACoeffs(TrangeLow,
                                                   ThermoDataLow['Cp'],
                                                   ThermoDataLow['H'],
                                                   ThermoDataLow['S'])
        NasaHighTCoeffs = nasafitter.fitNASACoeffs(TrangeHigh,
                                                    ThermoDataHigh['Cp'],
                                                    ThermoDataHigh['H'],
                                                    ThermoDataHigh['S'])

        return self._nasaOutToFormattedDict(Tlow,Tmid,Thigh,P,
                                            NasaLowTCoeffs,NasaHighTCoeffs)

    @staticmethod
    def _thermoOutToFormattedDict(T,P,H,S,U,G,Cp,Cv):
        FormattedThermoDict = {
            'RequestedTemperature': {'value': T, 'unit': 'K'},
            'RequestedPressure': {'value': P, 'unit': 'Pa'},
            'Enthalpy': {'value': H, 'unit': 'kJ/mol'},
            'InternalEnergy': {'value': U, 'unit': 'kJ/mol'},
            'Entropy': {'value': S, 'unit': 'kJ/mol/K'},
            'GibbsEnergy': {'value': G, 'unit': 'kJ/mol'},
            'HeatCapacityAtConstPressure': {'value': Cp, 'unit': 'kJ/mol/K'},
            'HeatCapacityAtConstVolume': {'value': Cv, 'unit': 'kJ/mol/K'}
        }
        return FormattedThermoDict

    @staticmethod
    def _nasaOutToFormattedDict(Tlow,Tmid,Thigh,P,NasaLowTCoeffs,NasaHighTCoeffs):
        FormattedThermoDict = {
            'LowTemperature': {'value': Tlow, 'unit': 'K'},
            'MidTemperature': {'value': Tmid, 'unit': 'K'},
            'HighTemperature': {'value': Thigh, 'unit': 'K'},
            'RequestedPressure': {'value': P, 'unit': 'Pa'},
            'LowTemperatureCoefficients': NasaLowTCoeffs,
            'HighTemperatureCoefficients':NasaHighTCoeffs,
            'NasaChemkinBlock': nasawriter.writeNasaChemkinBlock(Tlow,Tmid,Thigh,P,
                                            NasaLowTCoeffs,NasaHighTCoeffs)
        }
        return FormattedThermoDict

    def _getSpThermoAtTP(self,T,P):
        return {'S': self.getSpEntropySTHD(T,P),
                'H': self.getSpEnthalpySTHD(T),
                'Cp': self.getSpHeatCapacityCpSTHD(T),
                'Cv': self.getSpHeatCapacityCvSTHD(T),
                'U': self.getSpInternalEnergySTHD(T),
                'G': self.getSpGibbsEnergySTHD(T)}

    def _getSpThermoAtTrangeP(self,Trange,P):
        S= H= Cp= Cv= U= G= []

        for Ti in Trange:
            ThermoData = self._getSpThermoAtTP(Ti,P)

            S.append(ThermoData['S'])
            H.append(ThermoData['H'])
            Cp.append(ThermoData['Cp'])
            Cv.append(ThermoData['Cv'])
            U.append(ThermoData['U'])
            G.append(ThermoData['G'])

        return {'S': S,
                'H': H,
                'Cp': Cp,
                'Cv': Cv,
                'U': U,
                'G': G}

    def _getRotConstFromImom(self, InertiaMom):
        RotConst = [] # B in 1/m
        for I in InertiaMom:
            if I>0.0: # I in kg*m^2
                RotConst.append(p.BI_pref/I)
        self.RotConst = RotConst

    def _getImomFromRotConstant(self):
        InertiaMom = [] # I in kg*m^2
        if len(self.RotConst)>0:
            for B in self.RotConst:
                if B>0.0: # B in 1/m
                    InertiaMom.append(p.BI_pref/B)
        return InertiaMom

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
    defaultProps['ElecLvL']=[]
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

def checkDataConsistency(dict):
    if warnings:
        currentFreqNr = len(dict['VibFreq'])
        requiredFreqNr1 = int(3*getElementsNr(dict['Comp'])-5)
        requiredFreqNr2 = int(3*getElementsNr(dict['Comp'])-6)
        # Check nr of frequencies
        if dict['GeomType']==0 and currentFreqNr>0:
            print('Warning: Frequencies should not be defined for atomic species: '+ dict['Name'])
        if dict['GeomType']==1:
            if currentFreqNr < requiredFreqNr1:
                print('Warning: Species ' + dict['Name'] + ' has too few frequencies: ' + str(currentFreqNr) + '/' + str(requiredFreqNr1))
            elif currentFreqNr > requiredFreqNr1:
                print('Warning: Species ' + dict['Name'] + ' has too many frequencies: ' + str(currentFreqNr) + '/' + str(requiredFreqNr1))
        if dict['GeomType']==2:
            if currentFreqNr < requiredFreqNr2:
                print('Warning: Species ' + dict['Name'] + ' has too few frequencies: ' + str(currentFreqNr) + '/' + str(requiredFreqNr2))
            elif currentFreqNr > requiredFreqNr2:
                print('Warning: Species ' + dict['Name'] + ' has too many frequencies: ' + str(currentFreqNr) + '/' + str(requiredFreqNr2))

def CreateChemSpecFromDict(dict):
    # Get defaults
    def_dict = getDefaultProps()

    # Update dict with any missing keys
    for key, value in def_dict.items():
        if key not in dict:
            dict[key] = value

    # checkDataConsistency(dict)
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


def GetThermoData(T,Spec,unit,inclNasa=True,inclFitNasa=True,inclSthdNasaDiff=True,inclFitNasaNasaDiff=True,
                  inclSthdNasaFitNasaDiff = True):
    ncols = 6
    dataitems = ['S', 'H', 'Cp', 'Cv', 'U', 'G']
    dataunits = ['Entropy', 'Energy', 'HeatCap', 'HeatCap', 'Energy', 'Energy']
    DataHeaders = ['T ' + unit['Temperature'][0]]
    for i,item in enumerate(dataitems):
        DataHeaders.append(item + ' sthd ' + unit[dataunits[i]][0])

    if inclNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' nasa ' + unit[dataunits[i]][0])

    if inclFitNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' fitnasa ' + unit[dataunits[i]][0])

    if inclSthdNasaDiff and inclNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' abs sthd nasa diff ' + unit[dataunits[i]][0])

    if inclFitNasaNasaDiff and inclFitNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' abs fitnasa nasa diff ' + unit[dataunits[i]][0])

    if inclSthdNasaFitNasaDiff and inclFitNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' abs sthd fitnasa diff ' + unit[dataunits[i]][0])


    Data = np.zeros([len(T),len(DataHeaders)])
    for i,Ti in enumerate(T):
        nc = 6
        Data[i][0] = Ti*unit['Temperature'][1]+unit['Temperature'][2]
        Data[i][1] = Spec.getSpEntropySTHD(Ti)*unit['Entropy'][1]+unit['Entropy'][2]
        Data[i][2] = Spec.getSpEnthalpySTHD(Ti)*unit['Energy'][1]+unit['Energy'][2]
        Data[i][3] = Spec.getSpHeatCapacityCpSTHD(Ti)*unit['HeatCap'][1]+unit['HeatCap'][2]
        Data[i][4] = Spec.getSpHeatCapacityCvSTHD(Ti)*unit['HeatCap'][1]+unit['HeatCap'][2]
        Data[i][5] = Spec.getSpInternalEnergySTHD(Ti)*unit['Energy'][1]+unit['Energy'][2]
        Data[i][6] = Spec.getSpGibbsEnergySTHD(Ti)*unit['Energy'][1]+unit['Energy'][2]

        if inclNasa:
            Data[i][nc+1] = Spec.getSpEntropyNASA(Ti)*unit['Entropy'][1]+unit['Entropy'][2]
            Data[i][nc+2] = Spec.getSpEnthalpyNASA(Ti)*unit['Energy'][1]+unit['Energy'][2]
            Data[i][nc+3]  = Spec.getSpHeatCapacityCpNASA(Ti)*unit['HeatCap'][1]+unit['HeatCap'][2]
            Data[i][nc+4]  = Spec.getSpHeatCapacityCvNASA(Ti)*unit['HeatCap'][1]+unit['HeatCap'][2]
            Data[i][nc+5]  = Spec.getSpInternalEnergyNASA(Ti)*unit['Energy'][1]+unit['Energy'][2]
            Data[i][nc+6]  = Spec.getSpGibbsEnergyNASA(Ti)*unit['Energy'][1]+unit['Energy'][2]
            nc = nc + 6

        if inclFitNasa:
            Data[i][nc+1] = Spec.getSpEntropyNASA(Ti,inclFitNasa)*unit['Entropy'][1]+unit['Entropy'][2]
            Data[i][nc+2] = Spec.getSpEnthalpyNASA(Ti,inclFitNasa)*unit['Energy'][1]+unit['Energy'][2]
            Data[i][nc+3] = Spec.getSpHeatCapacityCpNASA(Ti,inclFitNasa)*unit['HeatCap'][1]+unit['HeatCap'][2]
            Data[i][nc+4] = Spec.getSpHeatCapacityCvNASA(Ti,inclFitNasa)*unit['HeatCap'][1]+unit['HeatCap'][2]
            Data[i][nc+5] = Spec.getSpInternalEnergyNASA(Ti,inclFitNasa)*unit['Energy'][1]+unit['Energy'][2]
            Data[i][nc+6] = Spec.getSpGibbsEnergyNASA(Ti,inclFitNasa)*unit['Energy'][1]+unit['Energy'][2]
            nc = nc + 6

        if inclSthdNasaDiff and inclNasa:
            Data[i][nc+1] = abs(Data[i][1]-Data[i][7])
            Data[i][nc+2] = abs(Data[i][2]-Data[i][8])
            Data[i][nc+3] = abs(Data[i][3]-Data[i][9])
            Data[i][nc+4] = abs(Data[i][4]-Data[i][10])
            Data[i][nc+5] = abs(Data[i][5]-Data[i][11])
            Data[i][nc+6] = abs(Data[i][6]-Data[i][12])
            nc = nc + 6

        if inclFitNasaNasaDiff and inclFitNasa and inclNasa:
            Data[i][nc+1] = abs(Data[i][13]-Data[i][7])
            Data[i][nc+2] = abs(Data[i][14]-Data[i][8])
            Data[i][nc+3] = abs(Data[i][15]-Data[i][9])
            Data[i][nc+4] = abs(Data[i][16]-Data[i][10])
            Data[i][nc+5] = abs(Data[i][17]-Data[i][11])
            Data[i][nc+6] = abs(Data[i][18]-Data[i][12])
            nc = nc + 6

        if inclSthdNasaFitNasaDiff and inclFitNasa:
            shift = 0
            if inclNasa: shift = 6
            Data[i][nc+1] = abs(Data[i][1]-Data[i][7+shift])
            Data[i][nc+2] = abs(Data[i][2]-Data[i][8+shift])
            Data[i][nc+3] = abs(Data[i][3]-Data[i][9+shift])
            Data[i][nc+4] = abs(Data[i][4]-Data[i][10+shift])
            Data[i][nc+5] = abs(Data[i][5]-Data[i][11+shift])
            Data[i][nc+6] = abs(Data[i][6]-Data[i][12+shift])

    return Data,DataHeaders

def getSpByNameFromList(SpList,aName):
    rsp = SpList[0]
    for sp in SpList:
        if sp.Name == aName:
            rsp = sp
            break
    return rsp

def getElementsNr(Comp):
    nEl = 0
    for i in range(1,len(Comp),2):
        nEl = nEl + int(Comp[i])
    return nEl


def _strInputToList(inputStr,type,delim=','):
     return [type(rc) for rc in inputStr.split(delim)]

def _strInputToListOfLists(inputStr,type,delim=',',length=2):
    if inputStr=='':
        return []
    inputStr=inputStr.split(delim)
    if len(inputStr) % length != 0:
        raise stdcerr.InvalidInput()

    outerList = []
    innerList = []
    for i, value in enumerate(inputStr):
        if i % length != 0:
            innerList.append(type(value))
        else:
            outerList.append(innerList)
            innerList = []
    return outerList
