import numpy as np
import stdc.utils.utils as utl
import stdc.utils.params as p
from stdc.utils.geomtypes import GeomTypes
import stdc.errorhandling.exceptions as stdcerr
import stdc.thermocalculator.sthdcalculator as sthd
import stdc.thermocalculator.partitionfunc as partf
import stdc.utils.nasafitter as nasafitter
import stdc.utils.nasablockwriter as nasawriter
import stdc.utils.diagnostics as diagn
import stdc.unitconverter.unitconverter as unitconv
import textwrap

#======================================================
#                 Chemical species class
#======================================================
# default temperature ranges
defaultTrange = "298.15,300,400,500,600,700,800,900,1000,1200,1500,1700,2000,2500,3000,3500,4000,4500,5000"
defaultNasaFitTemps = "298.15,1000,5000"
warnings = True

FREQ_CONV = unitconv.convertFrequencyUnitsToSI('CM^-1')
ROT_CONST_CONV = unitconv.convertEnergyMoleculeUnitsToSI('GHZ')* \
                 unitconv.convertEnergyMoleculeUnitsToSI('1/M',-1.0)
MOL_WEIGHT_CONV = unitconv.convertMassUnitsToSI('AMU',)
ELEC_EN_CONV = unitconv.convertEnergyMoleculeUnitsToSI('HA')
LVL_EN_CONV = unitconv.convertEnergyMoleculeUnitsToSI('CM^-1')

TP_THERMODATA = 'Thermodynamic data for a single T, P point'
TRANGE_P_THERMODATA = 'Thermodynamic data over a selected T range at a single P point'
NASA_THERMODATA = 'Fitted NASA polynomials'

class ChemSpecies:
    def __init__(self,
                 chem_formula,
                 spin_mult,
                 mol_weight,
                 sym_number="1",
                 rot_constants="",
                 frequencies="",
                 freq_scale_factor="1.0",
                 elec_levels="",
                 temperature="298.15",
                 pressure="101325.0",
                 enthalpy_ref_temp="298.15",
                 enthalpy_ref=None,
                 temperature_range=defaultTrange,
                 fit_nasa_temperatures=defaultNasaFitTemps,
                 fit_nasa=True,
                 dev_nasa_high_coeffs=None,
                 dev_nasa_low_coeffs=None,
                 dev_nasa_trange=None,
                 dev_enthref_from_nasa="0",
                 dev_enthref_nasa_temp="298.15",
                 dev_output_file=None):
        #   properties set based on constructor arguments
        #--------------------------------------------------
        # attribute name                               SI units
        self.ChemFormula = chem_formula                      #
        self.SpinMult = int(spin_mult)                       #     [-]
        self.MolWt = float(mol_weight)*MOL_WEIGHT_CONV       #     [kg] AMU - kg
        self.SymNr = int(float(sym_number))                         #     [-]
        self.RotConst = _strInputToList(
                            rot_constants,
                            type=float,
                            multiplier=ROT_CONST_CONV)       #     [1/m]  GHZ -> 1/m
        self.VibFreq = _strInputToList(
                            frequencies,
                            type=float,
                            multiplier=FREQ_CONV)            #     [1/s]  1/CM -> 1/s
        self.VibFreqScaleFactor = float(freq_scale_factor)   #     [-]
        self.ElecLvL = _strInputToListOfLists(
                            elec_levels,
                            types=[float,float],
                            multipliers=[1.0,LVL_EN_CONV])  #     [-,J]  1/cm -> J
        self.EnthRef = float(enthalpy_ref) \
                        if enthalpy_ref is not None \
                        else enthalpy_ref                    #     [J/mol]
        self.EnthRefTemp = float(enthalpy_ref_temp)          #     [K]
        #  Reuquested output temp and press + temp range for extra output
        #--------------------------------------------------
        self.RequestedTemp = float(temperature)              #     [K]
        self.RequestedPressure = float(pressure)             #     [Pa]
        self.RequestedTrange = _strInputToList(
                            temperature_range,
                            type=float)                      #     [n2 x K]
        self.ThermoData = {
            TP_THERMODATA: {},
            TRANGE_P_THERMODATA: {},
            NASA_THERMODATA: {}
        }
        #  NASA polynomials fitting - extra output
        #--------------------------------------------------
        self.FitNasa = fit_nasa
        self.FittedLowNasaCoeffs = []                     #     [7x-]
        self.NasaPolynomialsData = {}               #
        self.FittedHighNasaCoeffs = []                    #     [7x-]
        self.NasaComment = 'STHD'                   #
        self.NasaPhase = 'G'                        #
        self.NasaFitTemps = _strInputToList(
                            fit_nasa_temperatures,
                            type=float)             #     [K,K,K]
        # developer only inputs
        #--------------------------------------------------
        self.DevHighNasaCoeffs= dev_nasa_high_coeffs \
                                if dev_nasa_high_coeffs is None \
                                else _strInputToList( \
                                dev_nasa_high_coeffs, \
                                type=float)
        self.DevLowNasaCoeffs= dev_nasa_low_coeffs \
                                if dev_nasa_low_coeffs is None \
                                else _strInputToList( \
                                dev_nasa_low_coeffs, \
                                type=float)
        self.DevNasaTemps= dev_nasa_trange \
                                if dev_nasa_trange is None \
                                else _strInputToList( \
                                dev_nasa_trange,
                                type=float)
        self.DevEnthRefFromNasa= bool(int(dev_enthref_from_nasa))
        self.DevEnthRefNasaTemp = float(dev_enthref_nasa_temp)
        self.DevOutFile= dev_output_file
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
        if self.DevEnthRefFromNasa:
            self.EnthRef = self.getSpEnthalpyNASA(self.DevEnthRefNasaTemp,useFittedNasa=False)
        self._getSpHcorrSTHD()

    #======================================================
    #            Methods
    #======================================================

    def getThermoData(self):
        self._getRequestedTPPointData()
        self._getRequestedTrangeData()
        if self.FitNasa: self._getNasaPolynomialsData()

    def getSpPartFunc(self,T,P=p.Pref):
        q = partf.molecular_partfunc(self.MolWt,self.GeomType,
           self.SymNr,self.RotTemp,self.VibTemp,self.ElecLvL,T,P)
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

    def getSpGibbsEnergySTHD(self,T,P=p.Pref):
        S = self.getSpEntropySTHD(T,P)
        H = self.getSpEnthalpySTHD(T)
        G = H-T*S
        return G

    def getSpEntropyNASA(self,T,useFittedNasa=True):
        if useFittedNasa:
            S = utl.getEntropy(self.FittedLowNasaCoeffs,self.FittedHighNasaCoeffs,self.NasaFitTemps,T)
        else:
            S = utl.getEntropy(self.DevLowNasaCoeffs,self.DevHighNasaCoeffs,self.DevNasaTemps,T)
        return S

    def getSpInternalEnergyNASA(self,T,useFittedNasa=True):
        if useFittedNasa:
            U = utl.getInternalEnergy(self.FittedLowNasaCoeffs,self.FittedHighNasaCoeffs,self.NasaFitTemps,T)
        else:
            U = utl.getInternalEnergy(self.DevLowNasaCoeffs,self.DevHighNasaCoeffs,self.DevNasaTemps,T)
        return U

    def getSpHeatCapacityCvNASA(self,T,useFittedNasa=True):
        if useFittedNasa:
            Cv = utl.getHeatCapacityCv(self.FittedLowNasaCoeffs,self.FittedHighNasaCoeffs,self.NasaFitTemps,T)
        else:
            Cv = utl.getHeatCapacityCv(self.DevLowNasaCoeffs,self.DevHighNasaCoeffs,self.DevNasaTemps,T)
        return Cv

    def getSpHeatCapacityCpNASA(self,T,useFittedNasa=True):
        if useFittedNasa:
            Cp = utl.getHeatCapacityCp(self.FittedLowNasaCoeffs,self.FittedHighNasaCoeffs,self.NasaFitTemps,T)
        else:
            Cp = utl.getHeatCapacityCp(self.DevLowNasaCoeffs,self.DevHighNasaCoeffs,self.DevNasaTemps,T)
        return Cp

    def getSpEnthalpyNASA(self,T,useFittedNasa=True):
        if useFittedNasa:
            H = utl.getEnthalpy(self.FittedLowNasaCoeffs,self.FittedHighNasaCoeffs,self.NasaFitTemps,T)
        else:
            H = utl.getEnthalpy(self.DevLowNasaCoeffs,self.DevHighNasaCoeffs,self.DevNasaTemps,T)
        return H

    def getSpGibbsEnergyNASA(self,T,useFittedNasa=True):
        if useFittedNasa:
            G = utl.getGibbsEnergy(self.FittedLowNasaCoeffs,self.FittedHighNasaCoeffs,self.NasaFitTemps,T)
        else:
            G = utl.getGibbsEnergy(self.DevLowNasaCoeffs,self.DevHighNasaCoeffs,self.DevNasaTemps,T)
        return G

    def outputDiagnosticFile(self):
        diagn.writeThermoDatCsvFile(self)

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
            if len(self.RotConst) == 1:
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
            requiredFreqNum = int(3*self.AtomsNum-5)
            requiredRotConstNum = 1
            if rotConstNum != requiredRotConstNum:
                raise stdcerr.InvalidInput(textwrap.dedent(f"""
                        Error: Linear species requires exactly {requiredRotConstNum} non-zero rotational constants,
                        but {rotConstNum} were given."""))
            if freqNum != requiredFreqNum:
                raise stdcerr.InvalidInput(textwrap.dedent(f"""
                        Error: Linear species requires exactly {requiredFreqNum} non-zero frequnecies,
                        but {freqNum} were given."""))
        else:
            requiredFreqNum = int(3*self.AtomsNum-6)
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
        u_fact = unitconv.convertEnergyMoleculeUnitsToSI('1/M') # 1/m => J
        u_fact = u_fact * unitconv.convertEnergyMoleculeUnitsToSI('K', -1.0) # J=>K
        for B in self.RotConst:
            if B > 0.0:
                RotTemp.append(B*u_fact)
            else:
                raise stdcerr.InvalidInput()
        self.RotTemp = RotTemp

    def _setVibTemp(self):
        VibTemp = [] # in K
        u_fact = unitconv.convertEnergyMoleculeUnitsToSI('1/S') # 1/s => J
        u_fact = u_fact * unitconv.convertEnergyMoleculeUnitsToSI('K', -1.0) # J=>K
        for vib in self.VibFreq:
            if vib > 0.0:
                VibTemp.append(vib*u_fact)
            else:
                raise stdcerr.InvalidInput()
        self.VibTemp= VibTemp

    def _getSpHcorrSTHD(self):
        self.EnthRefCorr = 0.0
        if self.EnthRef is not None:
            H1 = sthd.getEnthalpy(self.GeomType,self.VibTemp,self.ElecLvL,self.EnthRefTemp)
            self.EnthRefCorr = self.EnthRef-H1

    def _getRequestedTPPointData(self):
        T = self.RequestedTemp
        P = self.RequestedPressure
        ThermoData= self._getSpThermoAtTP(T,P)
        ThermoData['T'] = T
        ThermoData['P'] = P

        self.ThermoData[TP_THERMODATA] = self._thermoOutToFormattedDict(ThermoData)

    def _getRequestedTrangeData(self):
        T = self.RequestedTrange
        P = self.RequestedPressure

        ThermoData= self._getSpThermoAtTrangeP(T,P)
        ThermoData['T'] =T
        ThermoData['P'] =P

        self.ThermoData[TRANGE_P_THERMODATA] = self._thermoOutToFormattedDict(ThermoData)

    def _getNasaPolynomialsData(self):
        self._fitNasaPolynomials()
        self.ThermoData[NASA_THERMODATA] = self._nasaOutToFormattedDict()

    def _fitNasaPolynomials(self):
        # By default the Trange is subdivided into 20 temperatures
        nT = 20

        Pref = self.RequestedPressure
        Tlow = self.NasaFitTemps[0]
        Tmid = self.NasaFitTemps[1]
        Thigh = self.NasaFitTemps[2]

        TrangeLow = np.linspace(Tlow,Tmid,nT)
        TrangeHigh = np.linspace(Tmid,Thigh,nT)

        ThermoDataLow = self._getSpThermoAtTrangeP(TrangeLow,Pref)
        ThermoDataHigh = self._getSpThermoAtTrangeP(TrangeHigh,Pref)

        NasaLowTCoeffs = nasafitter.fitNASACoeffs(TrangeLow,
                                                   ThermoDataLow['Cp'],
                                                   ThermoDataLow['H'],
                                                   ThermoDataLow['S'])
        NasaHighTCoeffs = nasafitter.fitNASACoeffs(TrangeHigh,
                                                    ThermoDataHigh['Cp'],
                                                    ThermoDataHigh['H'],
                                                    ThermoDataHigh['S'])

        self.FittedLowNasaCoeffs = NasaLowTCoeffs
        self.FittedHighNasaCoeffs = NasaHighTCoeffs

    @staticmethod
    def _thermoOutToFormattedDict(ThermoData):
        for key, value in ThermoData.items():
            if key == "H" or key == "G" or key == "U":
                mult=1e-3
            else:
                mult = 1.0
            if isinstance(value, list):
                ThermoData[key] = [f"{x*mult:.2f}" for x in value]
            else:
                ThermoData[key] = f"{value*mult:.2f}"

        FormattedThermoDict = {
            'Temperature': {'value': ThermoData['T'], 'unit': 'K'},
            'Pressure': {'value': ThermoData['P'], 'unit': 'Pa'},
            'Enthalpy': {'value': ThermoData['H'], 'unit': 'kJ/mol'},
            'Internal energy': {'value': ThermoData['U'], 'unit': 'kJ/mol'},
            'Entropy': {'value': ThermoData['S'], 'unit': 'J/mol/K'},
            'Gibbs energy': {'value': ThermoData['G'], 'unit': 'kJ/mol'},
            'Heat capacity at constant pressure': {'value': ThermoData['Cp'], 'unit': 'J/mol/K'},
            'Heat capacity at constant volume': {'value': ThermoData['Cv'], 'unit': 'J/mol/K'}
        }
        return FormattedThermoDict

    def _nasaOutToFormattedDict(self):
        formula = self.ChemFormula
        composition = []
        for key, value in self.AtomsCounts.items():
            composition.append(key)
            composition.append(value)
        Pref = self.RequestedPressure
        Tlow = self.NasaFitTemps[0]
        Tmid = self.NasaFitTemps[1]
        Thigh = self.NasaFitTemps[2]
        NasaLowTCoeffs = self.FittedLowNasaCoeffs
        NasaHighTCoeffs = self.FittedHighNasaCoeffs

        FormattedThermoDict = {
            'Pressure': {'value': f"{Pref:.2f}", 'unit': 'Pa'},
            'NASA low temperature': {'value': f"{Tlow:.2f}", 'unit': 'K'},
            'NASA medium temperature': {'value': f"{Tmid:.2f}", 'unit': 'K'},
            'NASA high temperature': {'value': f"{Thigh:.2f}", 'unit': 'K'},
            'NASA low temperature coefficients': [f"{x:.5e}" for x in NasaLowTCoeffs],
            'NASA high temperature coefficients':[f"{x:.5e}" for x in NasaHighTCoeffs],
            'Chemkin-style NASA polynomials data': nasawriter.writeNasaChemkinBlock(
                                    formula,composition,Tlow,Tmid,Thigh,
                                    NasaLowTCoeffs,NasaHighTCoeffs)
        }
        return FormattedThermoDict

    def _getSpThermoAtTP(self,T,P):
        return {'S': self.getSpEntropySTHD(T,P),
                'H': self.getSpEnthalpySTHD(T),
                'Cp': self.getSpHeatCapacityCpSTHD(T),
                'Cv': self.getSpHeatCapacityCvSTHD(T),
                'U': self.getSpInternalEnergySTHD(T),
                'G': self.getSpGibbsEnergySTHD(T,P)}

    def _getSpThermoAtTrangeP(self,Trange,P):
        S= []
        H= []
        Cp= []
        Cv= []
        U= []
        G= []

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

def _strInputToList(inputStr,type,delim=',',multiplier=1.0):
    if inputStr.strip()=="":
        return []
    else:
        return [type(type(rc)*multiplier) for rc in inputStr.split(delim)]

def _strInputToListOfLists(inputStr,types,delim=',',length=2,multipliers=None):
    if inputStr.strip()=='':
        return []
    if multipliers is None:
        multipliers = [1.0]*length
    if len(types) == 1:
        types = [types[0]]*length
    inputStr=inputStr.split(delim)
    if len(inputStr) % length != 0:
        raise stdcerr.InvalidInput()

    outerList = []
    innerList = []
    j = 0
    for value in inputStr:
        innerList.append(types[j](types[j](value)*multipliers[j]))
        j+=1
        if (j+1) % (length+1) == 0:
            outerList.append(innerList)
            innerList = []
            j = 0
    return outerList
