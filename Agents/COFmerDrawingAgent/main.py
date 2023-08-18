from Algorithms.operations import run_cofmer_pipeline
import csv
import re
import os


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

def construct_pattern(assemblyModel, component_num):
    construction_steps = {}
    product_counter = 1
    component_counters = [0, 0, 0]
    current_product = f"Component_1_Copy_1"
    component_counters[0] += 1
    
    placement_stage = "white_after_red"

    while sum(component_counters) < sum([assemblyModel[f"Component_{i} Copies"] for i in range(1, component_num + 1)]):
        if placement_stage == "white_after_red":
            for _ in range(3 - component_counters[2]):
                if component_counters[2] < assemblyModel["Component_3 Copies"]:
                    next_component = f"Component_3_Copy_{component_counters[2] + 1}"
                    construction_steps[f"Product_{product_counter}"] = [current_product, next_component]
                    current_product = f"Product_{product_counter}"
                    product_counter += 1
                    component_counters[2] += 1
            placement_stage = "pink_after_white"

        elif placement_stage == "pink_after_white":
            if component_counters[1] < assemblyModel["Component_2 Copies"]:
                next_component = f"Component_2_Copy_{component_counters[1] + 1}"
                construction_steps[f"Product_{product_counter}"] = [current_product, next_component]
                current_product = f"Product_{product_counter}"
                product_counter += 1
                component_counters[1] += 1
            placement_stage = "white_after_pink"

        elif placement_stage == "white_after_pink":
            if component_counters[2] < assemblyModel["Component_3 Copies"]:
                next_component = f"Component_3_Copy_{component_counters[2] + 1}"
                construction_steps[f"Product_{product_counter}"] = [current_product, next_component]
                current_product = f"Product_{product_counter}"
                product_counter += 1
                component_counters[2] += 1
            placement_stage = "red_after_white"

        elif placement_stage == "red_after_white":
            if component_counters[0] < assemblyModel["Component_1 Copies"]:
                next_component = f"Component_1_Copy_{component_counters[0] + 1}"
                construction_steps[f"Product_{product_counter}"] = [current_product, next_component]
                current_product = f"Product_{product_counter}"
                product_counter += 1
                component_counters[0] += 1
            placement_stage = "white_after_red"

    construction_steps["COFmer"] = current_product
    assemblyModel["ConstructionSteps"] = construction_steps

    return assemblyModel

def create_assembly_model(AM_string):
    am_match = re.match(r"(\w+)-\[\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\]n", AM_string)

    if not am_match:
        am_match = re.match(r"(\w+)-\[\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\]n", AM_string)
        component_num = 2
    else:
        component_num = 3

    assemblyModel = {}
    if am_match:
        assemblyModel["AM"] = AM_string
        
        for i in range(1, component_num + 1):
            assemblyModel[f"Component_{i}"], valency, _type = am_match.group(2 * i), *extract_valency_and_type(am_match.group(2 * i))
            assemblyModel[f"Component_{i} Copies"] = int(am_match.group(2 * i + 1))
            assemblyModel[f"Component_{i} Valency"] = valency
            assemblyModel[f"Component_{i} Type"] = _type
            assemblyModel[f"Component_{i} BS"] = None
        
        # Call the new construction function
        assemblyModel = construct_pattern(assemblyModel, component_num)
    else:
        print("The provided AM string does not match the expected format.")
    return assemblyModel



def create_assembly_model1(AM_string):
    am_match = re.match(r"(\w+)-\[\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\]n", AM_string)

    if not am_match:
        am_match = re.match(r"(\w+)-\[\(([\w:-]+)\)x(\d+)\(([\w:-]+)\)x(\d+)\]n", AM_string)
        
        component_num = 2
    else:
        component_num = 3

    assemblyModel = {}
    if am_match:
        assemblyModel["AM"] = AM_string
        
        for i in range(1, component_num + 1):
            assemblyModel[f"Component_{i}"], valency, _type = am_match.group(2 * i), *extract_valency_and_type(am_match.group(2 * i))
            assemblyModel[f"Component_{i} Copies"] = int(am_match.group(2 * i + 1))
            assemblyModel[f"Component_{i} Valency"] = valency
            assemblyModel[f"Component_{i} Type"] = _type
            assemblyModel[f"Component_{i} BS"] = None
        
        construction_steps = {}
        product_counter = 1
        component_counters = [0, 0, 0]
        current_product = f"Component_1_Copy_1"
        component_counters[0] += 1
        
        precursor_switch = False

        while sum(component_counters) < sum([assemblyModel[f"Component_{i} Copies"] for i in range(1, component_num + 1)]):
            # If precursor_switch is False, use Component 1, else use Component 2
            current_precursor = 1 if not precursor_switch else 2
            current_valency = assemblyModel[f"Component_{current_precursor} Valency"]

            # Add linkage components based on the valency of the current component
            for _ in range(current_valency):
                if component_counters[2] < assemblyModel["Component_3 Copies"]:
                    next_component = f"Component_3_Copy_{component_counters[2] + 1}"
                    construction_steps[f"Product_{product_counter}"] = [current_product, next_component]
                    current_product = f"Product_{product_counter}"
                    product_counter += 1
                    component_counters[2] += 1
                else:
                    break
            
            precursor_switch = not precursor_switch
            if precursor_switch and component_counters[0] < assemblyModel["Component_1 Copies"]:
                next_component = f"Component_1_Copy_{component_counters[0] + 1}"
                construction_steps[f"Product_{product_counter}"] = [current_product, next_component]
                current_product = f"Product_{product_counter}"
                product_counter += 1
                component_counters[0] += 1

            elif not precursor_switch and component_counters[1] < assemblyModel["Component_2 Copies"]:
                next_component = f"Component_2_Copy_{component_counters[1] + 1}"
                construction_steps[f"Product_{product_counter}"] = [current_product, next_component]
                current_product = f"Product_{product_counter}"
                product_counter += 1
                component_counters[1] += 1
        
        construction_steps["COFmer"] = current_product
        assemblyModel["ConstructionSteps"] = construction_steps
    else:
        print("The provided AM string does not match the expected format.")
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
                print(assembly_model_dict)
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