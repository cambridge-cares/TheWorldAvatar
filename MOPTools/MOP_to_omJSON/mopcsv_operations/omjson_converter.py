import csv 
import json

doc = """This is a converter of CSV files containing CBUs and MOPs.
Each individual entry is assigned a UUID. In the case of CBUs, parsing 
of xyz files takes place. The output are JSON files."""

def cbuCSVtoJSON(cbuIRICSVFilePath, cbuJSONFilePath): 
    with open(cbuIRICSVFilePath, "r", encoding='utf-8-sig') as csvCompletef: 
        csvReaderComplete = csv.DictReader(csvCompletef) 
        for row in csvReaderComplete :
            cbu = row['CBUCode']
            cbusubJSON = {
            "Empirical formula": row['Empirical formula'],
            "IRI": row['IRI'],
            "MolecularWeight": row['MolecularWeight'],
            "Formal charge": row['Formal charge'],
            "Spin multiplicity": row['Spin multiplicity'],
            "Planarity": row['Planarity'],
            "Modularity": row['Modularity'],
            "CentralUnit": row['CentralUnit'],
            "Spacer": row['Spacer'],
            "Substituent": row['Substituent'],
            "BindingUnit": row['BindingUnit']}
            outCBUs = json.dumps(cbusubJSON, indent=4)
            jsonoutput = open(cbuJSONFilePath+cbu+'.qc.json', 'w') 
            jsonoutput.write(outCBUs)

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
                "Mops_Chemical_Building_Units": row['Mops_Chemical_Building_Units']}
                out = json.dumps(outMOPs, indent=4)
                jsonoutput = open(mopJSONFilePath+str(lcount)+'.qc.json', 'w') 
                jsonoutput.write(out)
                lcount+=1