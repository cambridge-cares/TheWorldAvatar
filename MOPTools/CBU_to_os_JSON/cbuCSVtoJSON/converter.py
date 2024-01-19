import csv 
import json
import re
from CBU_to_os_JSON.cbuCSVtoJSON.printJSONdecision import printJSON

def cbuCSVtoQCJSON(cbuCSVFilePath, xyzInputCBU, speciesJSONFilePath):
    '''This function converts every row of the CBU.csv file into separate JSON file.'''
    data = {}
    rowoutSpecies = {}
    with open(cbuCSVFilePath, "r", encoding='utf-8-sig') as csvComp: 
        csvReaderComplete = csv.DictReader(csvComp) 
        decision = None
        for row in csvReaderComplete:
            inchi_string = row['InChi']
            spin_number = row['Spin multiplicity']
            decision = printJSON(inchi_string, spin_number)
            if decision is True:
                cbu = row['CBUCode']
                xyzCBU = xyzInputCBU+"\\"+cbu+'.xyz'
                data = parseXYZ(xyzCBU)
                row.update(data)
                # Depending on the desired json files one can furhter alter the output list
                rowoutSpecies.update({
                "Empirical formula": row['Empirical formula'],
                "InChi": row['InChi'],
                "Smiles": row['Smiles'],
                "MolecularWeight": row['MolecularWeight'],
                "Formal charge": row['Formal charge'],
                "Spin multiplicity": row['Spin multiplicity'],
                "Atom types": row['Atom types'],
                "Geometry": row['Geometry']
                })
                outspecies = json.dumps(rowoutSpecies, indent=4)
                jsonoutput = open(speciesJSONFilePath+'\\'+cbu+'.qc.json', 'w') 
                jsonoutput.write(outspecies)
            if decision is False:
                cbu = row['CBUCode']
                pass
            print(cbu, "    ", decision)
        

def parseXYZ(xyzCBU):
    '''This function parses XYZ files and returs back a dictionarry corresponding to involved atom types and their coordinates.'''
    data = {}
    atoms = []
    atoms_coordinates = [] 
    with open(xyzCBU, "r", encoding='utf-8-sig') as xyzf:
        xyzrows = xyzf.readlines()[2:]
        for row in xyzrows:  
            rowCoordinates = re.split('\s+', row)
            atom = str(rowCoordinates[0])
            atoms.append(atom)
            atomTypes = {"Atom types":atoms}
            atom_coordinates = (rowCoordinates[1], rowCoordinates[2], rowCoordinates[3])
            atoms_coordinates.append(atom_coordinates)
            Geometry = {"Geometry":atoms_coordinates}
            data.update(atomTypes)
            data.update(Geometry)
        return data