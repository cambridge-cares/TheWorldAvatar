import json

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