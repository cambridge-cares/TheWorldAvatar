import csv 
import json
from MOP_to_omJSON.mopcsv_operations.query_functions import printJSON, run_iri_query

doc = """This is a converter of CSV files containing CBUs and MOPs.
Each individual entry is assigned a UUID. In the case of CBUs, parsing 
of xyz files takes place. The output are JSON files."""

def cbuCSVtoJSON(cbuIRICSVFilePath, cbuJSONFilePath): 
    with open(cbuIRICSVFilePath, "r", encoding='utf-8-sig') as csvCompletef: 
        csvReaderComplete = csv.DictReader(csvCompletef) 
        for row in csvReaderComplete :
            inchi = row['InChi']
            iri = run_iri_query(inchi)
            row['OntoSpecies_IRI'] = iri
            cbu = row['CBUCode']
            if iri is not None:
                print(cbu+"      "+iri)
                cbusubJSON = {
                "OntoSpecies_IRI": row['OntoSpecies_IRI'],
                "CBU_Formula": row['CBU_Formula'],
                "Binding_Site": row['Binding_Site'],
                "Binding_Site_Label": row['Binding_Site_Label'],
                "Binding_SiteCoordNumber": row['Binding_SiteCoordNumber'],
                "CoreLabel": row['CoreLabel'],
                "CoreSubstituentLabel": row['CoreSubstituentLabel'],
                "SpacerLabel": row['SpacerLabel'],
                "SpacerSubstituentLabel": row['SpacerSubstituentLabel'],
                "BindingDirection": row['BindingDirection'],
                "GenericUnitPlanarity": row['GenericUnitPlanarity'],
                "GenericUnitModularity": row['GenericUnitModularity']
                }
                outCBUs = json.dumps(cbusubJSON, indent=4)
                jsonoutput = open(cbuJSONFilePath+cbu+'.qc.json', 'w') 
                jsonoutput.write(outCBUs)
            else:
                print(cbu+"      "+"NO IRI FOUND")
                pass

def mopCSVtoJSON(mopCSVFilePath, cbuJSONFilePath, mopJSONFilePath):
    '''Converts the CBU.csv file to separate JSON files.'''
    with open(mopCSVFilePath, "r", encoding='utf-8-sig') as csvf: 
        csvReader = csv.DictReader(csvf) 
        lcount = 1
        for row in csvReader:     
            cbu1 = row['CBU1']
            cbu2 = row['CBU2']
            mop_weight = row['Mops_Molecular_Weight']
            mop_shape = row['Mops_Polyhedral_Shape_Symbol']
            mop_charge = row['Mops_Charge']
            mop_formula = row['Mops_Formula']
            decision = printJSON(mop_weight, mop_shape, mop_charge)
            if decision is True:
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
                    "Mops_Molecular_Weight": row['Mops_Molecular_Weight'],
                    "Mops_Molecular_Weight_Unit": row['Mops_Molecular_Weight_Unit'],
                    "Mops_Polyhedral_Shape": row['Mops_Polyhedral_Shape'],
                    "Mops_Polyhedral_Shape_Symbol": row['Mops_Polyhedral_Shape_Symbol'],
                    "Mops_Symmetry_Point_Group": row['Mops_Symmetry_Point_Group'],                
                    "Mops_Geometry": row['Mops_Geometry'],
                    "Mops_Charge": row['Mops_Charge'],
                    "Mops_Charge_Unit": row['Mops_Charge_Unit'],
                    "Mops_Formula": row['Mops_Formula'],
                    "Mops_Label": row['Mops_Label'],
                    "Mops_CavityVolume": row['Mops_CavityVolume'],
                    "Mops_CCDC_Number": row['Mops_CCDC_Number'],
                    "Mops_Reference_DOI": row['Mops_Reference_DOI'],
                    "Mops_Chemical_Building_Units": row['Mops_Chemical_Building_Units']}
                    out = json.dumps(outMOPs, indent=4)
                    jsonoutput = open(mopJSONFilePath+str(lcount)+'.ominp.json', 'w') 
                    jsonoutput.write(out)
                    lcount+=1
            if decision is False:
                pass
            print(mop_formula, "    ", decision)