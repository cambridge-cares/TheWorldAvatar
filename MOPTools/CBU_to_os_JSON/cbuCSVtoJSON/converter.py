import csv 
import json
import re
from cbuCSVtoJSON.printJSONdecision import printJSON

doc = """This is a converter of CSV files containing CBUs and MOPs.
Each individual entry is assigned a UUID. In the case of CBUs, parsing 
of xyz files takes place. The output are JSON files."""

def cbuCSVtoJSON(cbuCSVFilePath, xyzInputCBU, speciesJSONFilePath):
    '''Each row in the CBU.csv file is convered into separate JSON file.
    An XYZ file corresponding to the CBU is parsed and merged in the final JSON.'''
    #read csv file
    data = {}
    rowoutSpecies = {}
    with open(cbuCSVFilePath, "r", encoding='utf-8-sig') as csvCompletef: 
        #load csv file data using csv library's dictionary reader
        csvReaderComplete = csv.DictReader(csvCompletef) 
        decision = None
        for row in csvReaderComplete:
            inchi_string = row['InChi']
            decision = printJSON(inchi_string)
            if decision is True:
                cbu = row['CBUCode']
                xyzCBU = xyzInputCBU+"\\"+cbu+'.xyz'
                data = parseXYZ(xyzCBU)
                row.update(data)
                rowoutSpecies.update({"Empirical formula": row['Empirical formula'], "InChi": row['InChi'], "Smiles": row['Smiles'], "MolecularWeight": row['MolecularWeight'], "Formal charge": row['Formal charge'], "Spin multiplicity": row['Spin multiplicity'],"Atom types": row['Atom types'], "Geometry": row['Geometry']})
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
        for row in xyzf:  
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

#List of Running Functions. Addition of UUIDs preceeds the generation of the JSON Files.


