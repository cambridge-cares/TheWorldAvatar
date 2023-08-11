from Algorithms.operations import run_cofmer_pipeline
import csv
import re
import os

def create_assembly_model(AM_string):
    # Try to match with three components pattern
    am_match = re.match(r"(\w+)-\[\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\]n", AM_string)

    if am_match:
        component_num = 3
    else:
        # Try to match with two components pattern
        am_match = re.match(r"(\w+)-\[\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\]n", AM_string)
        component_num = 2

    assemblyModel = {}
    if am_match:
        assemblyModel["AM"] = AM_string
        assemblyModel["Component_1"] = am_match.group(2)
        assemblyModel["Component_1 Copies"] = int(am_match.group(3))
        assemblyModel["Component_1 BS"] = None
        assemblyModel["Component_2"] = am_match.group(4)
        assemblyModel["Component_2 Copies"] = int(am_match.group(5))
        assemblyModel["Component_2 BS"] = None

        if component_num == 3:
            assemblyModel["Component_3"] = am_match.group(6)
            assemblyModel["Component_3 Copies"] = int(am_match.group(7))
            assemblyModel["Component_3 BS"] = None

        # Constructing the ConstructionSteps
        construction_steps = {}
        component_1_counter = 1
        component_2_counter = 1
        component_3_counter = 1 if component_num == 3 else 0
        product_counter = 1

        if assemblyModel["Component_1 Copies"] > 0 and (component_num == 2 or assemblyModel["Component_3 Copies"] > 0):
            construction_steps[f"Product_{product_counter}"] = ["Component_1_Copy_1", f"Component_{component_num}_Copy_{component_3_counter if component_num == 3 else component_2_counter}"]
            component_1_counter += 1
            if component_num == 3:
                component_3_counter += 1
            else:
                component_2_counter += 1
            product_counter += 1

        while component_1_counter <= assemblyModel["Component_1 Copies"] or component_2_counter <= assemblyModel["Component_2 Copies"] or (component_num == 3 and component_3_counter <= assemblyModel["Component_3 Copies"]):
            if component_2_counter <= assemblyModel["Component_2 Copies"] and (component_num == 2 or component_3_counter <= assemblyModel["Component_3 Copies"]):
                construction_steps[f"Product_{product_counter}"] = [f"Product_{product_counter - 1}", f"Component_2_Copy_{component_2_counter}"]
                component_2_counter += 1
                product_counter += 1

            if component_1_counter <= assemblyModel["Component_1 Copies"] and (component_num == 2 or component_3_counter <= assemblyModel["Component_3 Copies"]):
                construction_steps[f"Product_{product_counter}"] = [f"Product_{product_counter - 1}", f"Component_1_Copy_{component_1_counter}"]
                component_1_counter += 1
                product_counter += 1

            if component_num == 3 and component_3_counter <= assemblyModel["Component_3 Copies"]:
                construction_steps[f"Product_{product_counter}"] = [f"Product_{product_counter - 1}", f"Component_3_Copy_{component_3_counter}"]
                component_3_counter += 1
                product_counter += 1

        construction_steps["COFmer"] = f"Product_{product_counter - 1}"
        
        assemblyModel["ConstructionSteps"] = construction_steps
    else:
        print("The provided AM string does not match the expected format.")
    #print(assemblyModel)
    return assemblyModel

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


def initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir):
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
                
                
                assembly_model_dict = create_assembly_model(assembly_model_string)
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
COFs_path = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\data_csv\\COFs.csv'
output_dir = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\output_dir\\'
#print(initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir))

def main():
    # your code here
    
    initial_dict_creation(COFs_path, linkage_path, precursor_path, output_dir)

if __name__ == '__main__':
    main()