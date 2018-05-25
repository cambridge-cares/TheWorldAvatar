import ElementsData as eld
import params as p
import numpy as np
import thermochemistry as sthd
import utilities as utl
import scipy.optimize as opt
import lmfit as lmf
from lxml import etree
import csv

#======================================================
#                 Chemical species class
#======================================================
class ChemSpecies:
    #--------------------------------------------------
    #            Constructor
    #--------------------------------------------------
    def __init__(self,aName="",aFormula="",aComp=[],aMolWt=0.0,aVibFreq=[],aVibScale=1.0,aSymNr=1.0,aGeom=[],
                aGeomType=2,aImom=[],aElecEn=0.0,aZPE=0.0,aSpinMult=1.0,aElecLvL=[[1.0,0.0]],aLowNasa=[],
                ahighNasa=[],aTrangeNasa=[],aEnthRefSource='#None',aEnthRefTemp=298.15,aEnthRef=0.0,aFitLowNasa=[],
                aFitHighNasa=[],aFitTrangeNasa=[]):
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
        self.HighNasa = ahighNasa                #     [7x-]
        self.TrangeNasa = aTrangeNasa            #     [K,K,K]
        self.EnthRefSource = aEnthRefSource      #     [-]
        self.EnthRef = aEnthRef                  #     [J/mol]
        self.EnthRefTemp = aEnthRefTemp          #     [K]
        self.FitLowNasa = aFitLowNasa
        self.FitHighNasa = aFitHighNasa
        self.FitTrangeNasa = aFitTrangeNasa
        #   properties set based on other properties
        #--------------------------------------------------
        self.RotConst = []                       #     [1/m]
        self.VibTemp = []                        #     [K]
        self.RotTemp = []                        #     [K]
        self.ElMolWt = []                        #     [kg]
        self.XYZ = []                            #     [m]
        self.EnthRefCorr = 0.0                   #     [J/mol]
        self = self.setDerivedProperties()

    #--------------------------------------------------
    #                 Methods
    #--------------------------------------------------

    #---------------------------------
    # Methods to set/get other fields
    #---------------------------------

    # set dependent chemspecies fields
    #---------------------------------
    def setDerivedProperties(self):
        # get molecular mass and elements mass vector
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
        u_fact = utl.convertMassUnitsToSI('amu') # amu => kg
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
        u_fact = utl.convertEnergyMoleculeUnitsToSI('1/m') # 1/m => J
        u_fact = u_fact * utl.convertEnergyMoleculeUnitsToSI('K', -1.0) # J=>K
        if len(self.RotConst)>0:
            for B in self.RotConst:
                RotTemp.append(B*u_fact)
        return RotTemp

    # get vibrational temperatures
    #--------------------------
    def getVibTemp(self):
        VibTemp = [] # in K
        u_fact = utl.convertEnergyMoleculeUnitsToSI('1/s') # 1/s => J
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

def readSpecXML(xmlfile):
    tree = etree.parse(xmlfile)
    root = tree.getroot()    
    nsm = root.nsmap


    Name = ""
    Formula = ""
    Composition = []
    VibFreq = []
    SymNr = 1.0
    Geom = []
    GeomType = 2
    InertiaMom = []
    SpinMult = 1.0

    nsm['def'] = nsm[None]
    del nsm[None]

    moduleInit = root.find(".//def:module[@dictRef='cc:initialization']",nsm)
    moleculeInit = moduleInit.find('.//def:molecule',nsm)    
    for property in moleculeInit.iterchildren():
        if 'atomArray' in property.tag:
            for elem in property.iterchildren():
                Composition.append(elem.attrib['elementType'] )
                Composition.append(str(int(float(elem.attrib['count']))))
        elif 'formula' in property.tag:
            Formula = ''.join(property.attrib['concise'].split())
            Name = Formula

    moduleFinit = root.find(".//def:module[@dictRef='cc:finalization']",nsm)
    for property in moduleFinit.iterchildren():
        if 'propertyList' in property.tag:
            for subprop in property.iterchildren():
                if 'vibrations' in subprop.attrib['dictRef']:
                    propchild = subprop.getchildren()[0]
                    VibFreqUnit = propchild.attrib['units'].split(':')[1]
                    if 'm-1' in VibFreqUnit:
                        VibFreqUnit = VibFreqUnit.replace('-1','^-1')
                    u_fact = utl.convertFrequencyUnitsToSI(VibFreqUnit)
                    VibFreq = [float(f)*u_fact for f in propchild.text.split()]
                elif 'rotational_symmetry' in subprop.attrib['dictRef']:
                    propchild = subprop.getchildren()[0]
                    SymNr = float(propchild.text)
                elif 'rotational_constants' in subprop.attrib['dictRef']:
                    propchild = subprop.getchildren()[0]
                    RotConstUnit = propchild.attrib['units'].split(':')[1]
                    u_fact1 = utl.convertEnergyMoleculeUnitsToSI(RotConstUnit,1.0) # 1/TIME,Hz,1/cm => J
                    u_fact2 = utl.convertEnergyMoleculeUnitsToSI('1/m',-1.0) # J => 1/m
                    u_fact = u_fact1*u_fact2
                    RotConst = [float(f)*u_fact for f in propchild.text.split()]
                    InertiaMom = getImomFromRotConstant(RotConst)
                elif 'geometry_type' in subprop.attrib['dictRef']:
                    propchild = subprop.getchildren()[0]
                    GeomTypeTxt =propchild.text
                    if GeomTypeTxt == 'nonlinear':
                        GeomType = 2
                    elif GeomTypeTxt == 'linear':
                        GeomType = 1
                    else:
                        GeomType = 0
        elif 'molecule' in property.tag:
            SpinMult = float(property.attrib['spinMultiplicity'])
            for subprop in property.iterchildren():
                if 'atomArray' in subprop.tag:
                    u_fact = utl.convertLengthUnitsToSI('A')
                    for subsubprop in subprop.iterchildren():
                        atom = subsubprop.attrib['elementType']
                        x = float(subsubprop.attrib['x3'])*u_fact
                        y = float(subsubprop.attrib['y3'])*u_fact
                        z = float(subsubprop.attrib['z3'])*u_fact

                        Geom.append([atom,x,y,z])

    rSpec = ChemSpecies(aName=Name,aFormula=Formula,aComp=Composition,aVibFreq=VibFreq,aSymNr=SymNr, \
                        aGeom=Geom,aGeomType=GeomType,aImom=InertiaMom,aSpinMult=SpinMult)

    return rSpec


def RunThermoScriptFromCmd(xml_file,out_datfile,out_csvfile,tfit_range,tcsv_range,href):

    Sp = readSpecXML(xml_file)
    if len(out_datfile.strip())==0:
        out_datfile = xml_file.replace('.xml','.dat')
    if len(out_csvfile.strip())==0:
        out_csvfile = xml_file.replace('.xml','.csv')
    if len(href)>0:
        Sp.EnthRefTemp = href[0]
        Sp.EnthRef = href[1]
        Sp.EnthRefCorr = Sp.getSpHcorrSTHD()

    if len(tfit_range)==0:
        tfit_range = [298.15,1000,2500]
    if len(tcsv_range)==0:
        tcsv_range = [298,300,400,500,600,700,800,900,1000,1200,1500,1700,2000,2500,3000]

    Sp = fitThermoDataNASA(Sp,tfit_range)
    writeNasaDatFile(Sp,out_datfile)
    writeThermoDatCsvFile(Sp,out_csvfile,T=tcsv_range,inclFitNasa=True,inclNasa=False)

def writeThermoDatCsvFile(Sp,outfile,T=[298,300,400,600,800,1000,1500,2000,2500,3000],inclFitNasa=True,inclNasa=False):
    unit=1
    ThData, ThDataHeaders = GetThermoData(T,Sp,unit,inclNasa,inclFitNasa)
    with open(outfile, 'w', newline='') as csvfile:
        wrt = csv.writer(csvfile, delimiter=',')
        wrt.writerow(ThDataHeaders)
        for row in ThData:
            wrt.writerow(row)
    csvfile.close()

def writeNasaDatFile(Sp,datfile):
    with open(datfile, 'w', newline='') as thdfile:
        thdfile.write("{:16}".format(Sp.Formula))    
        thdfile.write("{:8}".format('STHD'))
        for i in range(0,8):
            if (i+1)%2==0:
                if i>len(Sp.Composition)-1:
                    thdfile.write("{:2}".format(' '))
                else:                
                    thdfile.write("{0:3d}".format(int(Sp.Composition[i])))
            else:
                if i>len(Sp.Composition)-1:
                    thdfile.write("{:3}".format(' '))
                else:
                    thdfile.write("{:2}".format(Sp.Composition[i]))
        
        thdfile.write("{:1}".format('G'))
        thdfile.write("{0:10.2f}".format(Sp.FitTrangeNasa[0]))
        thdfile.write("{0:10.2f}".format(Sp.FitTrangeNasa[2]))    
        thdfile.write("{0:8.2f}".format(Sp.FitTrangeNasa[1]))    
        thdfile.write("{:6}".format(' '))
        thdfile.write("{:1}".format('1\n'))
        
        for i in range(0,5):
            thdfile.write("{0:15.8e}".format(Sp.FitHighNasa[i]))
        thdfile.write("{:4}".format(' '))
        thdfile.write("{:1}".format('2\n'))
        thdfile.write("{0:15.8e}".format(Sp.FitHighNasa[5]))
        thdfile.write("{0:15.8e}".format(Sp.FitHighNasa[6]))
        for i in range(0,3):
            thdfile.write("{0:15.8e}".format(Sp.FitLowNasa[i]))
        thdfile.write("{:4}".format(' '))
        thdfile.write("{:1}".format('3\n'))
        for i in range(3,7):
            thdfile.write("{0:15.8e}".format(Sp.FitLowNasa[i]))
        thdfile.write("{:19}".format(' '))
        thdfile.write("{:1}".format('4\n'))
    thdfile.close()


# Read chemspecies list from a file
#--------------------------------------------------
def readChemSpeciesFile(spFile,spName='ALL'):
    # Parse Frequency section in a file
    #--------------------------
    def readFrequencies(rf,key,parseVars):
        runit = getUnit(key,parseVars)
        u_fact = utl.convertFrequencyUnitsToSI(runit)
        freq = []
        while True:
            line = rf.readline()
            stop_parsing ,skip_line, line = checkLine(line,'EndFrequencies',
                                  parseVars['comment_char'],['r','s'],0)
            if stop_parsing: break
            if skip_line == False:
                if line[-1] == parseVars['data_delim']:
                    line = line[:-1]
                if line[0] == parseVars['data_delim']:
                    line = line[1:]
                line = line.split(parseVars['data_delim'])
                for items in line:
                    freq.append(float(items)*u_fact)
        return rf,freq
    # Parse Geometry section in a file
    #--------------------------
    def readGeometry(rf,key,parseVars):
        runit = getUnit(key,parseVars)
        u_fact = utl.convertLengthUnitsToSI(runit)
        geom = []
        while True:
            line = rf.readline()
            stop_parsing,skip_line,line = checkLine(line,'EndGeometry',
                                  parseVars['comment_char'],['r','s'],4)
            if stop_parsing: break
            if skip_line == False:
                geom.append([line[0], float(line[1])*u_fact,float(line[2])*u_fact,float(line[3])*u_fact])
        return rf,geom

    # Parse Electronic levels section in a file
    #--------------------------    
    def readElecLevels(rf,key,parseVars):
        runit = getUnit(key,parseVars)
        u_fact = utl.convertEnergyMoleculeUnitsToSI(runit)
        ElecLvl = []
        while True:
            #skip_line = False
            line = rf.readline()

            stop_parsing,skip_line,line = checkLine(line,'EndElecLevels',
                                  parseVars['comment_char'],['r','l','s'],2)
            if stop_parsing: break
            if skip_line == False:
                ElecLvl.append([float(line[0]),float(line[1])*u_fact])
        return rf,ElecLvl
    # Parse NASA thermdata section in a file
    #--------------------------    
    def readNasaThermCoeffs(rf,parseVars):
        nc = 0
        alow= []
        ahigh= []
        Trange=[]
        while True:
            line = rf.readline()
            stop_parsing,skip_line,line = checkLine(line,'EndNasaThermCoeffs',
                                  parseVars['comment_char'],['r','l','s'],0)
            if stop_parsing: break
            if skip_line == False:
                nc = nc + 1
                if nc == 1:
                    Trange.append(float(line[45:55]))
                    Trange.append(float(line[65:73]))
                    Trange.append(float(line[55:65]))
                else:
                    if line[0]!='-':
                        line = ' '+line
                    if nc == 2:
                        ahigh.append(float(line[0:15]))
                        ahigh.append(float(line[15:30]))
                        ahigh.append(float(line[30:45]))
                        ahigh.append(float(line[45:60]))
                        ahigh.append(float(line[60:75]))            
                    elif nc == 3:
                        ahigh.append(float(line[0:15]))
                        ahigh.append(float(line[15:30]))
                        alow.append(float(line[30:45]))
                        alow.append(float(line[45:60]))
                        alow.append(float(line[60:75]))
                    elif nc == 4:
                        alow.append(float(line[0:15]))
                        alow.append(float(line[15:30]))
                        alow.append(float(line[30:45]))
                        alow.append(float(line[45:60]))
        return rf,alow,ahigh,Trange
    # Parse Enthalpy Source section in a file
    #--------------------------    
    def readEnthalpyRef(rf,val,parseVars):
        EnthRefTemp = 298.15
        EnthRef = 0.0
        EnthSource = val
        while True:
            line = rf.readline()
            stop_parsing,skip_line,line = checkLine(line,'EndEnhtalpyRef',
                                  parseVars['comment_char'],['r','s'],2)
            if stop_parsing: break
            if skip_line == False:
                key = line[0]
                value = line[1]
                if key == 'EnthRefSource':
                    EnthSource = value
                elif 'EnthRefTemp' in key:
                    runit = getUnit(key,parseVars)
                    u_fact = utl.convertTemperatureUnitsToSI(runit)
                    EnthRefTemp = float(value)*u_fact
                elif 'EnthRef' in key and EnthSource!='#NASA':
                    runit = getUnit(key,parseVars)
                    u_fact = utl.convertEnergyMoleUnitsToSI(runit)
                    EnthRef = float(value)*u_fact
        return rf,EnthSource,EnthRefTemp,EnthRef
    # Get unit from a keyword
    #--------------------------
    def getUnit(key,parseVars):
        key = key.split(parseVars['start_bracket'])
        key = key[1].split(parseVars['end_bracket'])
        key = key[0]
        runit = key
        return runit
    # check and prepare file line
    #--------------------------
    def checkLine(line,token,com_char,line_op,check_len):
        skip_line = False
        stop_parsing = False
        if not line: stop_parsing = True
        if token in line: stop_parsing = True

        if stop_parsing==False:
            if utl.compareList('s',line_op):
                line = line.strip()
            if utl.compareList('l',line_op):
                line = line.lstrip()
            if utl.compareList('r',line_op):
                line = line.rstrip('\n')

            if len(line)==0:
                skip_line = True
            elif line[0] == com_char:
                skip_line = True

            if check_len>0:
                line = line.split()
                if len(line)<check_len:
                    skip_line = True
        return stop_parsing,skip_line,line
    #--------------------------

    #----------------------------------------------------
    # main body of the "readChemSpeciesFile" function
    #----------------------------------------------------
    parseVars={} 
    parseVars['data_delim']      = ','
    parseVars['start_bracket']  = '['
    parseVars['end_bracket']  = ']'
    parseVars['comment_char']    = '!'
    
    #set default values
    aName = ""
    aFormula = ""
    aComposition = []
    aMolWt = 0.0
    aVibFreq = []
    aVibScale = 1.0
    aSymNr = 1.0
    aGeom = []
    aGeomType = 2
    aInertiaMom = []
    aRotConst = []
    aElecEn = 0.0
    aZPE = 0.0
    aSpinMult = 1.0
    aElecLvL = [[1.0,0.0]]
    alowNasa = []
    ahighNasa = []
    aTrangeNasa = []

    rSpecList= []
    stop_parsing = False
    with open(spFile, 'r') as f:
        while True:
            line = f.readline()
            if not line: break
            skip_line = False
            line = line.rstrip('\n')
            line = line.strip()
            if len(line) == 0:
                skip_line = True
            elif line[0] == parseVars['comment_char']:
                skip_line = True
            if skip_line == False:
                line = line.split()
                key = line[0]
                if len(line)>1:
                    value = line[1]
                if 'SPECIES' in key:                    
                    aName = value
                    if spName.upper() !='ALL':
                        if spName.upper()!=aName.upper():
                            while True:
                                line = f.readline()
                                if not line: break
                                line = line.rstrip('\n')
                                line = line.strip()
                                if line =='END':
                                    break
                        else:
                            stop_parsing = True
                elif 'Formula' in key:
                    aFormula = value
                elif 'Composition' in key:                    
                    value = value.split(parseVars['data_delim'])
                    aComposition = value
                elif 'Frequencies[' in key:
                    f,aVibFreq = readFrequencies(f,key,parseVars)
                elif 'FreqScalingFactor' in key:
                    aVibScale = float(value)
                elif 'SymmetryNr' in key:
                    aSymNr = float(value)
                elif 'GeomType' in key:
                    if value == 'atomic':
                        aGeomType = 0
                    elif value == 'linear':
                        aGeomType = 1
                    else:
                        aGeomType = 2
                elif 'Geometry[' in key:
                    f,aGeom = readGeometry(f,key,parseVars)
                elif 'SpinMultiplicity' in key:
                    aSpinMult = float(value)
                elif 'ElecEnergy' in key:
                    runit = getUnit(key,parseVars)
                    u_fact = utl.convertEnergyMoleUnitsToSI(runit)
                    aElecEn = float(value)*u_fact
                elif 'InertiaMom' in key:
                    aInertiaMom= []
                    if len(aRotConst) == 0:
                        runit = getUnit(key,parseVars)
                        u_fact = utl.convertInertiaUnitsToSI(runit)
                        value = value.split(parseVars['data_delim'])
                        aInertiaMom = [float(value[0])*u_fact,float(value[1])*u_fact,float(value[2])*u_fact]
                        #get units right
                    else:
                        print('Ignoring InertiaMom input as RotConsts already given')
                elif 'RotConstants' in key:
                    aRotConst = []
                    if len(aInertiaMom) == 0:
                        runit = getUnit(key,parseVars)
                        u_fact1 = utl.convertEnergyMoleculeUnitsToSI(runit,1.0) # 1/TIME,Hz,1/cm => J
                        u_fact2 = utl.convertEnergyMoleculeUnitsToSI('1/m',-1.0) # J => 1/m
                        u_fact = u_fact1*u_fact2
                        value = value.split(parseVars['data_delim'])
                        for v in value:
                            aRotConst.append(float(v)*u_fact)
                        aInertiaMom = getImomFromRotConstant(aRotConst)
                        #get units right
                    else:
                        print('Ignoring RotConstants input as InertiaMom already given')
                elif 'ZeroPointEnergy' in key:
                    runit = getUnit(key,parseVars)
                    u_fact = utl.convertEnergyMoleculeUnitsToSI(runit)
                    aZPE = float(value)*u_fact
                elif 'ElecLevels' in key:
                    f,aElecLvL = readElecLevels(f,key,parseVars)
                elif 'NasaThermCoeffs' in key:
                    Nasa = []
                    f,alowNasa,ahighNasa,aTrangeNasa = readNasaThermCoeffs(f,parseVars)
                elif 'EnhtalpyRef' in key:
                    f,aEnthSource,aEnthRefTemp,aEnthRef = readEnthalpyRef(f,value,parseVars)
                elif 'END' in key:
                    rSpec = ChemSpecies(aName,aFormula,aComposition,aMolWt,aVibFreq,aVibScale,aSymNr,aGeom,
                            aGeomType,aInertiaMom,aElecEn,aZPE,aSpinMult,aElecLvL,alowNasa,ahighNasa,aTrangeNasa,
                            aEnthSource,aEnthRefTemp,aEnthRef)
                    #add species to a list
                    rSpecList.append(rSpec)
                    if stop_parsing:
                        break
                    #set keywords to defaults
                    aName = ""
                    aFormula = ""
                    aComposition = []
                    aMolWt = 0.0
                    aVibFreq = []
                    aVibScale = 1.0
                    aSymNr = 1.0
                    aGeom = []
                    aGeomType = 2
                    aInertiaMom = []
                    aElecEn = 0.0
                    aZPE = 0.0
                    aSpinMult = 1.0
                    aElecLvL = [[1.0,0.0]]
                    alowNasa = []
                    ahighNasa = []
                    aTrangeNasa = []
    return rSpecList

def getRotConstFromImom(aImom):
    RotConst = [] # B in 1/m
    if len(aImom)>0:
        for I in aImom:
            if I>0.0: # I in kg*m^2                
                RotConst.append(p.BI_pref/I)
    return RotConst

def getImomFromRotConstant(aRotConst):
    InertiaMom = [] # I in kg*m^
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


def fitThermoDataNASA(Spec,Trange=[298.15,1000,2500]):
    def f(params,x,y):        
        a=[]
        a.append(params['a0'])
        a.append(params['a1'])
        a.append(params['a2'])
        a.append(params['a3'])
        a.append(params['a4'])
        a.append(params['a5'])
        a.append(params['a6'])

        Cp_eq = a[0]           + a[1]*x     + a[2]*x**2     + a[3]*x**3     + a[4]*x**4
        H_eq  = a[0]           + a[1]/2.0*x + a[2]/3.0*x**2 + a[3]/4.0*x**3 + a[4]/5.0*x**4 + a[5]/x
        S_eq  = a[0]*np.log(x) + a[1]*x     + a[2]/2.0*x**2 + a[3]/3.0*x**3 + a[4]/4.0*x**4 + a[6]

        residual = np.zeros([3,len(x)])
        residual[0][:] = y[0][:]-Cp_eq
        residual[1][:] = y[1][:]-H_eq
        residual[2][:] = y[2][:]-S_eq
        return residual

    def findNASACoeffs(Spec,xdata):
        x0= [1.0e+00, 1.0e-03, 1.0e-07, 1.0e-10, 1.0e-14, 1.0e+03, 1.0e+01]
        #x0= [1.0e+00, 1.0e+00, 1.0e+00, 1.0e+00, 1.0e+00, 1.0e+00, 1.0e+00]
        params = lmf.Parameters()
        params.add('a0',x0[0])
        params.add('a1',x0[1])
        params.add('a2',x0[2])
        params.add('a3',x0[3])
        params.add('a4',x0[4])
        params.add('a5',x0[5])
        params.add('a6',x0[6])
        ydata = np.zeros([3,len(xdata)])
        for i,xi in enumerate(xdata):
            Cpi = Spec.getSpHeatCapacityCpSTHD(xi)/p.R
            Hi = Spec.getSpEnthalpySTHD(xi)/p.R/xi
            Si = Spec.getSpEntropySTHD(xi)/p.R
            ydata[0][i] = Cpi
            ydata[1][i] = Hi
            ydata[2][i] = Si
        result = lmf.minimize(f, params, args=(xdata, ydata))
        aa= result.residual
        fitParams = []
        fitParams.append(result.params['a0'].value)
        fitParams.append(result.params['a1'].value)
        fitParams.append(result.params['a2'].value)
        fitParams.append(result.params['a3'].value)
        fitParams.append(result.params['a4'].value)
        fitParams.append(result.params['a5'].value)
        fitParams.append(result.params['a6'].value)
        return fitParams

    nT = 20
    Trange1 = np.linspace(Trange[0],Trange[1],nT)
    Trange2 = np.linspace(Trange[1],Trange[2],nT)
    
    resultLow = findNASACoeffs(Spec,Trange1)
    resultHigh = findNASACoeffs(Spec,Trange2)

    Spec.FitLowNasa = resultLow
    Spec.FitHighNasa = resultHigh  
    Spec.FitTrangeNasa = Trange

    return Spec


def getSpByNameFromList(SpList,aName):
    rsp = SpList[0]
    for sp in SpList:
        if sp.Name == aName:
            rsp = sp
            break

    return rsp