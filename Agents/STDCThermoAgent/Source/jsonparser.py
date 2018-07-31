import chemspecies as chs
import json
import utilities as utl

# Parse json file with molecular data
def readSpecJSON(jsonfile):
    with open(jsonfile) as f:
        data = json.load(f)

    dfPs= chs.getDefaultProps()

    results = {}
    results = data['results']['bindings']
    # loop through data blocks in json file
    for ritems in results:
        # each data block is a dictionary
        for key, values in ritems.items():
            # read molecule's name
            if key == 'moleculeName':
                dfPs['Name'] = values['value'].replace(' ','')
                dfPs['Formula'] = dfPs['Name']
            # read composition or coordinates
            elif key == 'atomName':
                atom = ritems['atomName']['value']
                atom = atom.split('#')[1]
                if 'coordinateX' in ritems:
                    x = float(ritems['coordinateX']['value'])
                    y = float(ritems['coordinateY']['value'])
                    z = float(ritems['coordinateZ']['value'])
                    dfPs['ElMolWt'].append(float(ritems['massValue']['value']))
                    dfPs['Geom'].append([atom,x,y,z])
                else:
                    dfPs['Comp'].append(atom)
                    dfPs['Comp'].append(str(int(float(ritems['atomNumber']['value']))))
            # read spin multiplicity
            elif key == 'spinMultiplicityValue':
                dfPs['SpinMult'] = float(ritems['spinMultiplicityValue']['value'])
            # read frequencies
            elif key == 'frequenciesValue':
                u_fact = 1.0
                if 'frequenciesUnit' in ritems:
                    VibFreqUnit = ritems['frequenciesUnit']['value'].split('/')[-1].upper()
                    if 'M-1' in VibFreqUnit:
                        VibFreqUnit = VibFreqUnit.replace('-1','^-1')
                    u_fact = utl.convertFrequencyUnitsToSI(VibFreqUnit)
                dfPs['VibFreq'] = [float(f)*u_fact for f in ritems['frequenciesValue']['value'].split()]
            # read GeomType
            elif key == 'geometryTypeValue':
                if ritems['geometryTypeValue']['value'] == 'nonlinear':
                    dfPs['GeomType']=2
                elif ritems['geometryTypeValue']['value'] == 'linear':
                    dfPs['GeomType']=1
                else:
                    dfPs['GeomType'] = 0
            # read RotConst
            elif key == 'rotationalConstantsValue':
                if 'rotationalConstantsUnit' in ritems:
                    RotConstUnit = ritems['rotationalConstantsUnit']['value'].split('#')[-1].upper()
                    if RotConstUnit == 'GIGAHERTZ': RotConstUnit = 'GHZ'
                    u_fact1 = utl.convertEnergyMoleculeUnitsToSI(RotConstUnit,1.0) # 1/TIME,Hz,1/cm => J
                    u_fact2 = utl.convertEnergyMoleculeUnitsToSI('1/M',-1.0) # J => 1/m
                    u_fact = u_fact1*u_fact2
                    RotConst = [float(f)*u_fact for f in ritems['rotationalConstantsValue']['value'].split()]
                    dfPs['Imom'] = chs.getImomFromRotConstant(RotConst)
            # read SymNr
            elif key == 'rotationalSymmetryNumber':
                dfPs['SymNr'] = float(ritems['rotationalSymmetryNumber']['value'])
            # read MetaData
            elif key == 'programName':
                dfPs['MetaData']['programName'] = ritems['programName']['value']
                dfPs['MetaData']['programVersion'] = ritems['programVersion']['value']
                dfPs['MetaData']['runDate'] = ritems['runDate']['value']
            elif key == 'levelOfTheory':
                dfPs['MetaData']['levelOfTheory'] = ritems['levelOfTheory']['value']
                dfPs['MetaData']['basisSetValue'] = ritems['basisSetValue']['value']

    # Create Species
    rSpec = chs.CreateChemSpecFromDict(dfPs)
    return rSpec

def writeJsonOutFile(Sp,out_datfile):
    #create dictionary with Nasa polynomials data
    nasaDict = {}
    nasaDict['runDate'] = ''
    nasaDict['programName'] = ''
    nasaDict['programVersion'] = ''
    nasaDict['levelOfTheory'] = ''
    nasaDict['basisSetValue'] = ''

    nasaDict['Name'] = Sp.Name
    nasaDict['Comment'] = 'STHD'
    nasaDict['Composition'] = Sp.Composition
    nasaDict['Tmin'] = Sp.FitTrangeNasa[0]
    nasaDict['Tmid'] = Sp.FitTrangeNasa[1]
    nasaDict['Tmax'] = Sp.FitTrangeNasa[2]
    nasaDict['LowTcoeff'] = Sp.FitLowNasa
    nasaDict['highTcoeff'] = Sp.FitHighNasa
    nasaDict['Phase'] = 'G'
    if 'runDate' in Sp.MetaData : nasaDict['runDate'] = Sp.MetaData['runDate']
    if 'programName' in Sp.MetaData: nasaDict['programName'] = Sp.MetaData['programName']
    if 'programVersion' in Sp.MetaData: nasaDict['programVersion'] = Sp.MetaData['programVersion']
    if 'levelOfTheory' in Sp.MetaData: nasaDict['levelOfTheory'] = Sp.MetaData['levelOfTheory']
    if 'basisSetValue' in Sp.MetaData: nasaDict['basisSetValue'] = Sp.MetaData['basisSetValue']

    atomicMasses = []
    atoms = []
    for i,el in enumerate(Sp.ElMolWt):
        atoms.append(Sp.Geometry[i][0])
        atomicMasses.append(el)
    nasaDict['atomicMasses'] = [atoms, atomicMasses]

    with open(out_datfile, 'w') as outfile:
        json.dump(nasaDict, outfile)