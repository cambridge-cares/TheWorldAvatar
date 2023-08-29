from Algorithms.operations import run_cofmer_pipeline
from Data.assemblies import assembly_db
import csv
import re
import os

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

def extract_valency_and_type(component_string):
    linkage_match = re.match(r"L:(\d+)-\w+", component_string)
    if linkage_match:
        valency = int(linkage_match.group(1))
        return (valency, 'linkage')
    
    precursor_match = re.match(r"(\d+)-\w+", component_string)
    if precursor_match:
        valency = int(precursor_match.group(1))
        return (valency, 'precursor')
    
    return None, None

def initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir, assembly_db):
    precursor_values_1 = None
    precursor_values_2 = None
    with open(COFs_path, 'r', encoding='utf-8-sig') as cof_file:
        cof_reader = csv.DictReader(cof_file)

        for cof_row in cof_reader:
            try:
                assembly_model_string = cof_row['Assembly_Model']
                linkage_lfr = cof_row['Linkage']
                precursor_1 = cof_row['Precursor1']
                precursor_2 = cof_row['Precursor2'] if cof_row['Precursor2'] != '0' else None
                COF_Nr = cof_row['COF_Nr']
                output_directory_name = f"COF_{COF_Nr}"
                full_output_path = os.path.join(output_dir, output_directory_name)
                if not os.path.exists(full_output_path):
                    os.mkdir(full_output_path)

                linkages = {}
                with open(linkage_path, 'r', encoding='utf-8') as linkage_file:
                    linkage_reader = csv.DictReader(linkage_file)
                    for linkage_row in linkage_reader:
                        if linkage_row['Linkage'] == linkage_lfr:
                            #print(linkage_lfr)
                            dummies = [int(x) for x in linkage_row["Dummies"].replace('"', '').split(',')] if linkage_row["Dummies"] else []
                            bs_dict = {
                                "UnitFrom": linkage_row["UnitFrom"],
                                "bindingSite": linkage_row["bindingSite"],
                                "bsIndex": int(linkage_row["bsIndex"]),
                                'Dummies': dummies
                            }
                            
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
                #print(linkages)
                # Processing Precursors.csv for Precursor1
                with open(precursor_path, 'r', encoding='utf-8') as precursor_file:
                    precursor_reader = csv.DictReader(precursor_file)
                    for precursor_row in precursor_reader:
                        if precursor_row['Precursor'] == precursor_1:
                            precursor_values_1 = {
                                "Precursor": precursor_row["Precursor"],
                                "GBU": precursor_row["GBU"],
                                "ConstructingMol": precursor_row["ConstructingMol"],
                                "BS": [
                                    {
                                        "UnitFrom": precursor_row["UnitFrom"],
                                        "bindingSite": precursor_row["bindingSite"],
                                        "bsIndex": int(precursor_row["bsIndex"]) # Converting bsIndex to integer
                                    }
                                ]
                            }

                            break

                # Processing Precursors.csv for Precursor2, if exists
                if precursor_2:
                    with open(precursor_path, 'r', encoding='utf-8') as precursor_file:
                        precursor_reader = csv.DictReader(precursor_file)
                        for precursor_row in precursor_reader:
                            if precursor_row['Precursor'] == precursor_2:
                                precursor_values_2 = {
                                    "Precursor": precursor_row["Precursor"],
                                    "GBU": precursor_row["GBU"],
                                    "ConstructingMol": precursor_row["ConstructingMol"],
                                    "BS": [
                                        {
                                            "UnitFrom": precursor_row["UnitFrom"],
                                            "bindingSite": precursor_row["bindingSite"],
                                            "bsIndex": int(precursor_row["bsIndex"])
                                        }
                                    ]
                                }

                                break
                
                assembly_model_dict = assembly_db[assembly_model_string]
                #assembly_model_dict = create_assembly_model(assembly_model_string)
                #print(assembly_model_dict)
                componentTypeNumber = create_componentTypeNumber_dict(assembly_model_string)
                precursors = [precursor_values_1]
                if precursor_2 is not None:
                    precursors.append(precursor_values_2)
            
                input_dir = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\input_dir\\'
                
                #run_cofmer_pipeline(assembly_model_dict, componentTypeNumber, precursor_values_1, precursor_values_2, linkage, input_dir, full_output_path)
                run_cofmer_pipeline(assembly_model_dict, componentTypeNumber, precursors, linkage, input_dir, full_output_path)

            except Exception as e:
                print(f"Error processing line with COF_Nr {cof_row['COF_Nr']}: {str(e)}")
                continue


# Assuming the CSV file is located at 'data.csv'
linkage_path = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\data_csv\\Linkages.csv'
precursor_path = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\data_csv\\Precursors.csv'
COFs_path = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\data_csv\\COFsTestZone.csv'
output_dir = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\output_dir\\'
#print(initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir))

def main():
    # your code here
    
    initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir,assembly_db)

if __name__ == '__main__':
    main()