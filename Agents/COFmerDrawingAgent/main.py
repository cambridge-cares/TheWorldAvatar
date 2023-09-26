import os
import csv
import re
import traceback
from Algorithms.operations import run_cofmer_pipeline
from Data.assemblies import assembly_db

def create_componentTypeNumber_dict(assembly_model):
    componentTypeNumber = {"Precursor": 0, "Linkage": 0}

    # Extracting the components inside the brackets
    components = re.findall(r'\((.*?)\)', assembly_model)
    
    for component in components:
        if 'L:' in component:
            componentTypeNumber["Linkage"] += 1
        else:
            componentTypeNumber["Precursor"] += 1
            
    return componentTypeNumber

def initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir, assembly_db):
    
    with open(COFs_path, 'r', encoding='utf-8-sig') as cof_file:
        cof_reader = csv.DictReader(cof_file)

        for cof_row in cof_reader:
            try:
                assembly_model_string = cof_row['Assembly_Model']
                linkage_lfr = cof_row['Linkage']
                precursor_1 = cof_row['Precursor1']
                precursor_2 = cof_row['Precursor2'] if cof_row['Precursor2'] != '0' else None
                COF_Nr = cof_row['COF_Nr']
                print('Processing__'+COF_Nr)
                output_directory_name = f"COF_{COF_Nr}"
                full_output_path = os.path.join(output_dir, output_directory_name)
                if not os.path.exists(full_output_path):
                    os.mkdir(full_output_path)

                linkages = {}
                with open(linkage_path, 'r', encoding='utf-8') as linkage_file:
                    linkage_reader = csv.DictReader(linkage_file)
                    for linkage_row in linkage_reader:
                        if linkage_row['Linkage'] == linkage_lfr:
                            if linkage_row["Dummies"] != 'NA':
                                dummies_value = linkage_row.get("Dummies")
                                dummies = [int(x) for x in dummies_value.replace('"', '').split(',')] if dummies_value and dummies_value != "NA" else []
                                complementaries_value = linkage_row.get("Complementaries")
                                complementaries = [int(x) for x in complementaries_value.replace('"', '').split(',')] if complementaries_value and complementaries_value != "NA" else []
                                bs_dict = {
                                    "UnitFrom": linkage_row["UnitFrom"],
                                    "bindingSite": linkage_row["bindingSite"],
                                    "bsIndex": int(linkage_row["bsIndex"]),
                                    "Dentation": linkage_row["Dentation"],
                                    "Dummies": dummies,
                                    "Complementaries": complementaries
                                }
                            else:
                                bs_dict = {
                                    "UnitFrom": linkage_row["UnitFrom"],
                                    "bindingSite": linkage_row["bindingSite"],
                                    "bsIndex": int(linkage_row["bsIndex"]),
                                    "Dentation": linkage_row["Dentation"],
                                    "Dummies": [],
                                    "Complementaries": []}
                            
                            # Check if the linkage exists; if not, initialize it
                            if linkage_lfr not in linkages:
                                linkages[linkage_lfr] = {
                                    "Linkage": linkage_row["Linkage"],
                                    "GBU": linkage_row["GBU"],
                                    "ConstructingMol": linkage_row["ConstructingMol"],
                                    "BS": []                                    
                                }
                            
                            linkages[linkage_lfr]["BS"].append(bs_dict) # append the bs_dict to the list of BS dictionaries
                            
                linkage = linkages.get(linkage_lfr) # get the linkage, if it exists

                precursors = []
                
                with open(precursor_path, 'r', encoding='utf-8') as precursor_file:
                    precursor_reader = csv.DictReader(precursor_file)
                    
                    precursor_dict = {}  # Dictionary to hold the current precursor info
                    for precursor_row in precursor_reader:
                        if precursor_row['Precursor'] in [precursor_1, precursor_2]:
                            if precursor_row['Precursor'] not in precursor_dict:
                                # Create the precursor dictionary with default values
                                precursor_dict[precursor_row['Precursor']] = {
                                    "Precursor": precursor_row["Precursor"],
                                    "GBU": precursor_row["GBU"],
                                    "ConstructingMol": precursor_row["ConstructingMol"],
                                    "BS": []  # Placeholder for binding sites
                                    
                                }
                            dummies_str = precursor_row.get("Dummies", "").replace('"', '')
                            complementaries_str = precursor_row.get("Complementaries", "").replace('"', '')
                            if dummies_str and dummies_str != "NA":
                                dummies_list = [int(x) for x in dummies_str.split(',')]
                            else:
                                dummies_list = []
                            if complementaries_str and complementaries_str != "NA":
                                complementaries_list = [int(x) for x in complementaries_str.split(',')]
                            else:
                                complementaries_list = []                            
                            bs_dict = {
                                "UnitFrom": precursor_row["UnitFrom"],
                                "bindingSite": precursor_row["bindingSite"],
                                "bsIndex": int(precursor_row["bsIndex"]),
                                "Dummies": dummies_list,
                                "Dentation": precursor_row["Dentation"],
                                "Complementaries": complementaries_list
                            }
                            precursor_dict[precursor_row['Precursor']]["BS"].append(bs_dict)
                            
                    # Appending precursor dictionaries to the precursors list
                    if precursor_1 in precursor_dict:
                        precursors.append(precursor_dict[precursor_1])
                    if precursor_2 and precursor_2 in precursor_dict:
                        precursors.append(precursor_dict[precursor_2])
                        
               
                assembly_model_dict = assembly_db[assembly_model_string]
                componentTypeNumber = create_componentTypeNumber_dict(assembly_model_string)
            
                input_dir = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\input_dir\\'
                run_cofmer_pipeline(assembly_model_dict, componentTypeNumber, precursors, linkage, input_dir, full_output_path)

            except Exception as e:
                print(f"Error processing line with COF_Nr {cof_row['COF_Nr']}: {str(e)}")
                print(traceback.format_exc())  # This prints the traceback
                continue


# Assuming the CSV file is located at 'data.csv'
linkage_path = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\data_csv\\LinkagesTEST.csv'
precursor_path = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\data_csv\\PrecursorsTEST.csv'
COFs_path = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\data_csv\\COFs_CLEAN.csv'
output_dir = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\output_dir\\'
#print(initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir))

def main():
    # your code here
    initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir,assembly_db)

if __name__ == '__main__':
    main()