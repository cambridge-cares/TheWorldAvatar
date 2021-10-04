import csv 
import json
import re

doc = """This is a converter of CSV files containing CBUs and MOPs.
Each individual entry is assigned a UUID. In the case of CBUs, parsing 
of xyz files takes place. The output are JSON files."""

def cbuCSVtoJSON(cbuCSVFilePath, speciesJSONFilePath, cbuJSONFilePath):
    '''Each row in the CBU.csv file is convered into separate JSON file.
    An XYZ file corresponding to the CBU is parsed and merged in the final JSON.'''
    #read csv file
    data = {}
    rowoutSpecies = {}
    rowoutCBUs = {}
    with open(cbuCSVFilePath, "r", encoding='utf-8-sig') as csvCompletef: 
        #load csv file data using csv library's dictionary reader
        csvReaderComplete = csv.DictReader(csvCompletef) 
        for row in csvReaderComplete :
            cbu = row['CBUCode']
            #xyzCBU = 'C:\\Converter_CSV_JSON\\inputXYZ\\'+cbu+'.xyz'
            xyzCBU = 'C:\\Users\\ak2332\\Documents\\Converter_CSV_JSON\\01_CBU_IO\\inputXYZ\\'+cbu+'.xyz'
            data = parseXYZ(xyzCBU)
            row.update(data)
            rowoutSpecies.update({"Empirical formula": row['Empirical formula'], "InChi": row['InChi'], "Smiles": row['Smiles'], "MolecularWeight": row['MolecularWeight'], "Formal charge": row['Formal charge'], "Spin multiplicity": row['Spin multiplicity'],"Atom types": row['Atom types'], "Geometry": row['Geometry']})
            outspecies = json.dumps(rowoutSpecies, indent=4)
            jsonoutput = open(speciesJSONFilePath+cbu+'.qc.json', 'w') 
            jsonoutput.write(outspecies)
            rowoutCBUs.update({"Empirical formula": row['Empirical formula'], "InChi": row['InChi'], "Smiles": row['Smiles'], "MolecularWeight": row['MolecularWeight'], "Formal charge": row['Formal charge'], "Spin multiplicity": row['Spin multiplicity'],"Planarity": row['Planarity'],"Modularity": row['Modularity'],"CentralUnit": row['CentralUnit'],"Spacer": row['Spacer'],"Substituent": row['Substituent'], "BindingUnit": row['BindingUnit']})
            outCBUs = json.dumps(rowoutCBUs, indent=4)
            jsonoutput = open(cbuJSONFilePath+cbu+'.qc.json', 'w') 
            jsonoutput.write(outCBUs)

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

def mopCSVtoJSON(mopCSVFilePath, cbuJSONFilePath, mopJSONFilePath):
    '''Converts the CBU.csv file to separate JSON files.'''
    
    with open(mopCSVFilePath, "r", encoding='utf-8-sig') as csvf: 
        csvReader = csv.DictReader(csvf) 
        lcount = 1
        for row in csvReader:     
            cbu1 = row['CBU1']
            cbu2 = row['CBU2']
            cbuPath1 = cbuJSONFilePath+cbu1+'.qc.json'
            cbuPath2 = cbuJSONFilePath +cbu2+'.qc.json'
            
            with open(cbuPath1, 'r') as cbu1read:
                cbu1data = json.load(cbu1read)
                cbu1data['GenericUnitNumber'] = row['Number_CBU1']
            with open(cbuPath2, 'r') as cbu2read:
                cbu2data = json.load(cbu2read)
                cbu2data['GenericUnitNumber'] = row['Number_CBU2']
            row["Mops_Chemical_Building_Units"] = [cbu1data, cbu2data]

            outMOPs = {
            "Mops_Formula": row['Mops_Formula'],
            "Mops_Charge": row['Mops_Charge'],
            "Mops_Polyhedral_Shape": row['Mops_Polyhedral_Shape'],
            "Mops_Polyhedral_Shape_Symbol": row['Mops_Polyhedral_Shape_Symbol'],
            "Mops_Charge_Unit": row['Mops_Charge_Unit'],
            "Mops_Molecular_Weight": row['Mops_Molecular_Weight'],
            "Mops_Molecular_Weight_Unit": row['Mops_Molecular_Weight_Unit'],
            "Mops_CCDC_Number": row['Mops_CCDC_Number'],
            "Mops_CavityVolume": row['Mops_CavityVolume'],
            "Mops_Chemical_Building_Units": row['Mops_Chemical_Building_Units']
            }
            out = json.dumps(outMOPs, indent=4)
            jsonoutput = open(mopJSONFilePath+str(lcount)+'.qc.json', 'w') 
            jsonoutput.write(out)
            lcount+=1


#List of Paths. In case of change, they need to be updated. read csv file
operationsFolder = 'C:\\Users\\ak2332\\Documents\\Converter_CSV_JSON\\01_CBU_IO\\'
cbuCSVFilePath = operationsFolder+'CBU_Input\\CBUs_InChi.csv'
cbuJSONFilePath = operationsFolder+'CBU_Output\\CBU_'
speciesJSONFilePath = operationsFolder+'Species_Output\\Species_'
xyzPath = operationsFolder+'inputXYZ\\'
xyzMOPsPath = operationsFolder+'inputXYZ\\'
mopCSVFilePath = operationsFolder+'MOP_Input\\MOPs.csv'
mopJSONFilePath =  operationsFolder+'MOP_Output\\MOP_'


#List of Running Functions. Addition of UUIDs preceeds the generation of the JSON Files.
cbuCSVtoJSON(cbuCSVFilePath, speciesJSONFilePath, cbuJSONFilePath)
mopCSVtoJSON(mopCSVFilePath, cbuJSONFilePath, mopJSONFilePath)