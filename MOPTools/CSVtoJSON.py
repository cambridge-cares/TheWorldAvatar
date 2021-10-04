import rdkit
from rdkit import Chem
from rdkit.Chem import Draw
import csv 
import json
import uuid
import pandas as pd 
import re
#comment

doc = """This is a converter of CSV files containing CBUs and MOPs.
Each individual entry is assigned a UUID. In the case of CBUs, parsing 
of xyz files takes place. The output are JSON files."""

def cbuCSVtoJSON(cbuEditedCSVFilePath, cbuJSONFilePath):
    '''Each row in the CBU.csv file is convered into separate JSON file.'''
     
    #read csv file
    with open(cbuEditedCSVFilePath, "r", encoding='utf-8') as csvf: 
        #load csv file data using csv library's dictionary reader
        csvReader = csv.DictReader(csvf) 
        for row in csvReader:
            fileNaming = row['CBUCode']
            out = json.dumps(row, indent=4)
            jsonoutput = open(cbuJSONFilePath+fileNaming+'.json', 'w') 
            jsonoutput.write(out) 

def cbuCSVtoJSONComplete(cbuEditedCSVFilePath, cbuCompleteJSONPath):
    '''Each row in the CBU.csv file is convered into separate JSON file.
    An XYZ file corresponding to the CBU is parsed and merged in the final JSON.'''
    #read csv file
    data = {}
    with open(cbuEditedCSVFilePath, "r", encoding='utf-8') as csvCompletef: 
        #load csv file data using csv library's dictionary reader
        csvReaderComplete = csv.DictReader(csvCompletef) 
        for row in csvReaderComplete :
            cbu = row['CBUCode']
            xyzCBU = 'C:\\Converter_CSV_JSON\\inputXYZ\\'+cbu+'.xyz'
            #xyzCBU = 'C:\\Users\\ak2332\\Documents\\Programs_CambridgeHome\\Converter_CSV_JSON\\inputXYZ\\'+cbu+'.xyz'
            data = parseXYZ(xyzCBU)
            row.update(data)
            out = json.dumps(row, indent=4)
            jsonoutput = open(cbuCompleteJSONPath+cbu+'.json', 'w') 
            jsonoutput.write(out) 

def parseXYZ(xyzCBU):
    '''This function parses XYZ files and returs back a dictionarry corresponding to involved atom types and their coordinates.'''
    data = {}
    atoms = []
    atoms_coordinates = [] 
    with open(xyzCBU, "r", encoding='utf-8') as xyzf: 
        for row in xyzf:  
            rowCoordinates = re.split('\s+', row)
            atom = str(rowCoordinates[0])
            atoms.append(atom)
            atomTypes = {"Atom_types":atoms}
            atom_coordinates = (rowCoordinates[1], rowCoordinates[2], rowCoordinates[3])
            atoms_coordinates.append(atom_coordinates)
            Geometry = {"Geometry":atoms_coordinates}
            data.update(atomTypes)
            data.update(Geometry)
        return data

def addUUIDtoCBUCSV(cbuCSVFilePath, cbuEditedCSVFilePath):
    '''Adds a column with generated UUIDs to the CBUs.csv file.
    In the process it creates an edited temporary file.'''
    csv_input = pd.read_csv(cbuCSVFilePath)
    csv_input['UUID'] = [uuid.uuid4() for x in range(len(csv_input))]
    csv_input.to_csv(cbuEditedCSVFilePath, index=True)

def addUUIDtoMOPCSV(mopCSVFilePath, mopEditedCSVFilePath):
    '''Adds a column with generated UUIDs to the MOPs.csv file.
    In the process it creates an edited temporary file.'''
    csv_input = pd.read_csv(mopCSVFilePath)
    csv_input['UUID'] = [uuid.uuid4() for x in range(len(csv_input))]
    csv_input.to_csv(mopEditedCSVFilePath, index=True)

def mopCSVtoJSON(mopEditedCSVFilePath, mopJSONFilePath):
    '''Converts the CBU.csv file to separate JSON files.'''
    #read csv file
    with open(mopEditedCSVFilePath, "r", encoding='utf-8') as csvf: 
        #load csv file data using csv library's dictionary reader
        csvReader = csv.DictReader(csvf) 
        lcount = 1
        for row in csvReader:
            out = json.dumps(row, indent=4)
            cbu1json = []
            cbu2json = []
            cbu1 = row['CBU1']
            cbu2 = row['CBU2']
            cbuPath1 = f'C:\\Converter_CSV_JSON\\CBU_JSON_Output\\CBU_'+cbu1+'.json'
            cbuPath2 = f'C:\\Converter_CSV_JSON\\CBU_JSON_Output\\CBU_'+cbu2+'.json'     
            #cbuPath1 = f'C:\\Users\\ak2332\\Documents\\Programs_CambridgeHome\\Converter_CSV_JSON\\CBU_JSON_Output\\CBU_'+cbu1+'.json'
            #cbuPath2 = f'C:\\Users\\ak2332\\Documents\\Programs_CambridgeHome\\Converter_CSV_JSON\\CBU_JSON_Output\\CBU_'+cbu2+'.json'     
            with open(cbuPath1, 'r') as cbu1read:
                cbu1data = json.load(cbu1read)
                cbu1json.append(cbu1data)
                outcbu1 = json.dumps(cbu1json, indent=8)
            with open(cbuPath2, 'r') as cbu2read:
                cbu2data = json.load(cbu2read)
                cbu2json.append(cbu2data)
                outcbu2 = json.dumps(cbu2json, indent=8)
            jsonoutput = open(mopJSONFilePath+str(lcount)+'.json', 'w') 
            jsonoutput.write(out)
            jsonoutput.write(outcbu1)
            jsonoutput.write(outcbu2)
            lcount+=1

def cbuCSVtoJSON(cbuEditedCSVFilePath, cbuJSONFilePath):
    '''Each row in the CBU.csv file is convered into separate JSON file.'''
     
    #read csv file
    with open(cbuEditedCSVFilePath, "r", encoding='utf-8') as csvf: 
        #load csv file data using csv library's dictionary reader
        csvReader = csv.DictReader(csvf) 
        for row in csvReader:
            fileNaming = row['CBUCode']
            out = json.dumps(row, indent=4)
            jsonoutput = open(cbuJSONFilePath+fileNaming+'.json', 'w') 
            jsonoutput.write(out) 

def cbuCSVtoDraw(cbuSMILESCSVFilePath):
    '''Each row in the CBU.csv file is convered into separate JSON file.
    An XYZ file corresponding to the CBU is parsed and merged in the final JSON.'''
    with open(cbuSMILESCSVFilePath, "r", encoding='utf-8') as csvCompletef: 
        #load csv file data using csv library's dictionary reader
        csvReaderComplete = csv.DictReader(csvCompletef) 
        for row in csvReaderComplete :
            cbu = row['SMILES']
            cbuCode = row['CBUCode']
            cbuMol = Chem.MolFromSmiles(cbu)
            #InChiEntry = rdkit.Chem.inchi.MolToInchi(cbuMol)
            imagefile = 'C:\\Converter_CSV_JSON\\drawCBU\\'+cbuCode+'.png'
            rdkit.Chem.Draw.MolToFile(cbuMol, imagefile)
            #xyzCBU = 'C:\\Users\\ak2332\\Documents\\Programs_CambridgeHome\\Converter_CSV_JSON\\inputXYZ\\'+cbu+'.xyz'


#List of Paths. In case of change, they need to be updated. read csv file
#operationsFolder = 'C:\\Users\\ak2332\\Documents\\Programs_CambridgeHome\\Converter_CSV_JSON\\'
operationsFolder = 'C:\\Converter_CSV_JSON\\'
cbuCSVFilePath = operationsFolder+'CBU_CSV_Input\\CBUs.csv'
cbuEditedCSVFilePath = operationsFolder+'CBU_CSV_Input\\CBUs_Edited.csv'
cbuJSONFilePath = operationsFolder+'CBU_JSON_Output\\CBU_'
cbuCompleteJSONPath = operationsFolder+'CBU_JSON_CompleteOutput\\CBU_'
mopCSVFilePath = operationsFolder+'MOP_CSV_Input\\MOPs.csv'
mopEditedCSVFilePath = operationsFolder+'MOP_CSV_Input\\MOPs_Edited.csv'
mopJSONFilePath =  operationsFolder+'MOP_JSON_Output\\MOP_'
xyzPath = operationsFolder+'inputXYZ\\'
cbuSMILESCSVFilePath = operationsFolder+'CBU_CSV_Input\\CBUs_InChiEdited.csv'

#List of Running Functions. Addition of UUIDs preceeds the generation of the JSON Files.
addUUIDtoCBUCSV(cbuCSVFilePath, cbuEditedCSVFilePath)
addUUIDtoMOPCSV(mopCSVFilePath, mopEditedCSVFilePath)
cbuCSVtoJSON(cbuEditedCSVFilePath, cbuJSONFilePath)
mopCSVtoJSON(mopEditedCSVFilePath, mopJSONFilePath)
cbuCSVtoJSONComplete(cbuEditedCSVFilePath, cbuCompleteJSONPath)
cbuCSVtoDraw(cbuSMILESCSVFilePath)