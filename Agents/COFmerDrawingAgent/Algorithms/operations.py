from rdkit import Chem
from rdkit.Chem import rdchem
from rdkit.Chem import AllChem
import csv
import os
import json

# -------------------------------------- INPUTS ----------------------------------------------
# assemblyModel: A dictionary containing information about the molecular assembly
# componentTypeNumber: A dictionary containing the expected number of components of each type
# precursors: A list of precursor components
# linkages: A list of linkage components



# -------------------------------------- COMPONENT DIFFERENTIATOR ----------------------------------------------

def component_handler(assemblyModel, componentTypeNumber, precursors, linkages):
    components = {}
    result = component_am_mol_matcher(assemblyModel, componentTypeNumber, precursors=precursors, linkages=linkages)
    precursor_dict, linkage_dict = ordering_components(result)
    linkage_copies = dummify_linkages(linkage_dict)
    precursor_copies = dummify_precursors(precursor_dict)
    components.update(linkage_copies)
    components.update(precursor_copies)
    print(components)
    return components

def component_am_mol_matcher(assemblyModel, componentTypeNumber, *, precursors=None, linkages=None):
    # check if number of precursors match ComponentTypeNumber for Precursor
    if precursors is None and componentTypeNumber.get("Precursor", 0) > 0:
        return "Error: Number of Precursors does not match ComponentTypeNumber for Precursor"
    elif precursors is not None and componentTypeNumber.get("Precursor", 0) != len(precursors):
        return "Error: Number of Precursors does not match ComponentTypeNumber for Precursor"

    # check if number of linkages match ComponentTypeNumber for Linkage
    if linkages is None and componentTypeNumber.get("Linkage", 0) > 0:
        return "Error: Number of Linkages does not match ComponentTypeNumber for Linkage"
    elif linkages is not None and componentTypeNumber.get("Linkage", 0) != 1:
        return "Error: Number of Linkages does not match ComponentTypeNumber for Linkage"

    # check if components in assemblyModel match the precursors and linkages
    
    components = {k:v for k,v in assemblyModel.items() if "Component" in k}
    for precursor in precursors:     
        if precursor is not None and precursor.get("GBU", "") in components.values():
            gbu = precursor.get("GBU", "")
            precursor_info = {
                "ConstructingMol": precursor.get("ConstructingMol", ""),
                "BS": precursor.get("BS", [])
            }
            for k, v in components.items():
                if v == gbu:
                    components[k + " BS"] = precursor_info["BS"]
        components.update({k: precursor_info["ConstructingMol"] for k, v in components.items() if v == gbu})
            #print(components)
    for linkage in linkages: 
        #print(linkage)
        if linkage is not None and linkage.get("GBU", "") in components.values():
            gbu = linkage.get("GBU", "")
            linkage_info = {
                "ConstructingMol": linkage.get("ConstructingMol", ""),
                "BS": linkage.get("BS", [])
            }
            for k, v in components.items():
                if v == gbu:
                    components[k + " BS"] = linkage_info["BS"]
        components.update({k: linkage_info["ConstructingMol"] for k, v in components.items() if v == gbu})
    
    return components

def ordering_components(input_dict):
    precursor_dict = {}
    linkage_dict = {}
    
    # Get all the Component_names
    component_names = set(key.replace(" Copies", "").replace(" BS", "") for key in input_dict.keys() if 'Component' in key)
    
    # Create a dictionary for each Component_with its corresponding data
    for component_name in component_names:
        component_data = {}
        for key in input_dict.keys():
            if component_name in key:
                new_key = key.replace(component_name + " ", "")
                component_data[new_key] = input_dict[key]
        unit_from = component_data.get('BS', [{}])[0].get('UnitFrom', '')
        if unit_from == 'Precursor':
            precursor_dict[component_name] = component_data
        elif unit_from == 'Linkage':
            linkage_dict[component_name] = component_data
    
    return precursor_dict, linkage_dict

def dummify_linkages(component_dict):
    dummies_start = 90
    output_dict = {}
    total_dummies = sum([component_data['Copies'] * len(component_data['BS']) for component_data in component_dict.values()])
    for component_name, component_data in component_dict.items():
        component_copies = component_data['Copies']
        bs_data_sorted = sorted(component_data['BS'], key=lambda x: x['bsIndex'])
        for i in range(1, component_copies + 1):
            copy_name = f"{component_name}_Copy_{i}"
            input_file = component_data[component_name]
            output_file = f"{copy_name}.mol"
            bs_data_list = []
            for bs_data in bs_data_sorted:
                bs_index = bs_data['bsIndex']
                bs_data_copy = {'bindingSite': bs_data['bindingSite'], 'Dummies': []}
                for j in range(bs_index):
                    bs_data_copy['Dummies'].append(dummies_start)
                    dummies_start += 1
                bs_data_list.append(bs_data_copy)
            if copy_name in output_dict:
                output_dict[copy_name]['Linkage_BS'].extend(bs_data_list)
            else:
                output_dict[copy_name] = {'inputFile': input_file, 'outputFile': output_file, 'Linkage_BS': bs_data_list}
    return output_dict

def dummify_precursors(component_dict):
    dummies_start = 100
    output_dict = {}
    total_dummies = sum([component_data['Copies'] * len(component_data['BS']) for component_data in component_dict.values()])
    for component_name, component_data in component_dict.items():
        component_copies = component_data['Copies']
        bs_data_sorted = sorted(component_data['BS'], key=lambda x: x['bsIndex'])
        for i in range(1, component_copies + 1):
            copy_name = f"{component_name}_Copy_{i}"
            input_file = component_data[component_name]
            output_file = f"{copy_name}.mol"
            bs_data_list = []
            for bs_data in bs_data_sorted:
                bs_index = bs_data['bsIndex']
                bs_data_copy = {'bindingSite': bs_data['bindingSite'], 'Dummies': []}
                for j in range(bs_index):
                    bs_data_copy['Dummies'].append(dummies_start)
                    dummies_start += 1
                bs_data_list.append(bs_data_copy)
            if copy_name in output_dict:
                output_dict[copy_name]['Precursor_BS'].extend(bs_data_list)
            else:
                output_dict[copy_name] = {'inputFile': input_file, 'outputFile': output_file, 'Precursor_BS': bs_data_list}
    return output_dict

#----------------------COMPONENT ISTANCE MOLECULIZER-------------------------

def component_mol_handler(component_dict, input_dir, output_dir):
    for component_name, component_data in component_dict.items():
        input_file = os.path.join(input_dir, component_data['inputFile'])
        output_file = os.path.join(output_dir, component_data['outputFile'])
        dummies = None
        
        if 'Linkage_BS' in component_data:
            dummies = [d['Dummies'] for d in component_data['Linkage_BS']][0]
        elif 'Precursor_BS' in component_data:
            dummies = [d['Dummies'] for d in component_data['Precursor_BS']][0]
        
        with open(input_file, 'r') as f:
            mol_block = f.read()
        mol = Chem.MolFromMolBlock(mol_block)
        
        if dummies is not None:
            mol = dummy_atom_replacer(mol, dummies)
            
        with open(output_file, 'w') as f:
            f.write(Chem.MolToMolBlock(mol))
        component_mol_cleaner(output_dir)

def dummy_atom_replacer(mol, dummies):
    dummy_atom_index = 0
    for atom in mol.GetAtoms():
        if atom.GetAtomicNum() == 0:
            atom.SetAtomicNum(dummies[dummy_atom_index % len(dummies)])
            dummy_atom_index += 1

    return mol

def component_mol_cleaner(directory):
    for filename in os.listdir(directory):
        if filename.endswith('.mol'):
            file_path = os.path.join(directory, filename)
            with open(file_path, 'r') as f:
                mol_block = f.read()

            # Replace 'RDKit' with 'AK-CARES'
            mol_block = mol_block.replace('RDKit', 'AK-CARES')

            # Remove lines starting with V, A, or *
            mol_lines = mol_block.split('\n')
            cleaned_lines = [line for line in mol_lines if not line.startswith(('V', 'A', '*'))]
            cleaned_mol_block = '\n'.join(cleaned_lines)

            with open(file_path, 'w') as f:
                f.write(cleaned_mol_block)
                
#----------------------PRODUCT COMPONENT HANDLER -------------------------

def initial_product_instantiation(assembly_model, SingleComponent):
    product1 = assembly_model["ConstructionSteps"]["Product_1"]
    component1 = product1[0]
    component2 = product1[1]
    matching_dummies = []
    outputFile = "Product_1.mol"
    inputFile = [SingleComponent[component1]['outputFile'], SingleComponent[component2]['outputFile']]
    if "Linkage_BS" in SingleComponent[component2] and "Precursor_BS" in SingleComponent[component1]:
        for bs1 in SingleComponent[component1]["Precursor_BS"]:
            for bs2 in SingleComponent[component2]["Linkage_BS"]:
                if bs1["bindingSite"] == bs2["bindingSite"]:
                    matching_dummies = [bs1["Dummies"][0], bs2["Dummies"][0]]                   
                    bs1["Dummies"].remove(matching_dummies[0])
                    bs2["Dummies"].remove(matching_dummies[1])
                    Product_1 = {"outputFile": outputFile, "inputFile": inputFile, 
                                    "BindingDummies": matching_dummies, 
                                    "Precursor_BS": [bs1],
                                    "Linkage_BS": [bs2]}
                    SingleComponent['Product_1'] = Product_1      
        #break
    return SingleComponent

def product_instantiation(assembly_model, SingleComponent, product_name):
    product1 = assembly_model["ConstructionSteps"][product_name]
    print(product_name)
    component1 = product1[0]
    component2 = product1[1]
    
    matching_dummies = []
    outputFile = product_name+".mol"
    inputFile = [SingleComponent[component1]['outputFile'], SingleComponent[component2]['outputFile']]
    if "Linkage_BS" in SingleComponent[component2] and "Precursor_BS" in SingleComponent[component1]:
        for bs1 in SingleComponent[component1]["Precursor_BS"]:
            for bs3 in SingleComponent[component1]["Linkage_BS"]:
                for bs2 in SingleComponent[component2]["Linkage_BS"]:
                    if bs1["bindingSite"] == bs2["bindingSite"]:
                        matching_dummies = [bs1["Dummies"][0], bs2["Dummies"][0]]
                        if bs2["bindingSite"] == bs3["bindingSite"]:
                            bs23 = merging_dummies(bs2, bs3)
                            bs1["Dummies"].remove(matching_dummies[0])
                            bs23["Dummies"].remove(matching_dummies[1])
                            Product_1 = {"outputFile": outputFile, "inputFile": inputFile, 
                                "BindingDummies": matching_dummies, 
                                "Precursor_BS": [bs1],
                                "Linkage_BS": [bs23]}
                            SingleComponent[product_name] = Product_1          
    elif "Precursor_BS" in SingleComponent[component2] and "Linkage_BS" in SingleComponent[component1]:
        for bs1 in SingleComponent[component2]["Precursor_BS"]:
            for bs2 in SingleComponent[component1]["Linkage_BS"]:
                for bs3 in SingleComponent[component1]["Precursor_BS"]:
                    if bs1["bindingSite"] == bs2["bindingSite"]:
                        matching_dummies = [bs1["Dummies"][0], bs2["Dummies"][0]]
                        if bs1["bindingSite"] == bs3["bindingSite"]:
                            bs13 = merging_dummies(bs1, bs3)
                            bs13["Dummies"].remove(matching_dummies[0])
                            bs2["Dummies"].remove(matching_dummies[1])
                            Product_1 = {"outputFile": outputFile, "inputFile": inputFile, 
                                "BindingDummies": matching_dummies, 
                                "Precursor_BS": [bs13],
                                "Linkage_BS": [bs2]}
                            SingleComponent[product_name] = Product_1
    return SingleComponent

def merging_dummies(*dicts):
    merged_dict = {}
    for d in dicts:
        for key, value in d.items():
            if key in merged_dict:
                if isinstance(value, list):
                    merged_dict[key].extend(value)
                else:
                    merged_dict[key] = value
            else:
                merged_dict[key] = value
    merged_dict['Dummies'] = sorted(list(set(merged_dict['Dummies'])))
    return merged_dict

def looping_to_cofmer(assembly_model, initiation_single_component, second_initiation_step):
    result_dict = {}
    construction_steps = assembly_model["ConstructionSteps"]
    current_component = initiation_single_component
    for product_name, components in construction_steps.items():
        if product_name == "Product_1":
            continue  # Skip Product_1 since it's already initialized
        if product_name == "COFmer":
            continue  # Skip COFmer since it's already initialized
        temp_dict = second_initiation_step(assembly_model, current_component, product_name)
        #print(temp_dict)
        result_dict = {**result_dict, **temp_dict}
    return result_dict

#----------------------PRODUCT COMPONENT MOLECULIZER-------------------------

def product_mol_handler(input_dict):
    output_list = []
    
    # Loop through all keys in the dictionary
    for key in input_dict.keys():
        # Check if the key contains the word "Product_"
        if "Product_" in key:
            # Extract the relevant information from the dictionary
            input_files = input_dict[key]["inputFile"]
            
            output_file = r"C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\output_dir\\" + input_dict[key]["outputFile"]
            binding_dummies = input_dict[key]["BindingDummies"]
            # Generate the variables needed for combine_molecules
            input_component_1 = r"C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\output_dir\\" + input_files[0]
            input_component_2 = r"C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\output_dir\\" + input_files[1]
            binding_dummy_1 = binding_dummies[0]
            binding_dummy_2 = binding_dummies[1]
            product_mol_combiner(input_component_1, binding_dummy_1, input_component_2, binding_dummy_2, output_file)
            
            # Create a new dictionary to store the information for this key
            new_dict = {"inputFile": input_files, "outputFile": output_file, 
                        "BindingDummies": binding_dummies}
            
            # Add the new dictionary to the output list
            output_list.append(new_dict)
    
    return output_list

def product_mol_combiner(mol1_path, dummy1, mol2_path, dummy2, output_file):
    form_dummy1= '[#{}]'.format(dummy1)
    form_dummy2= '[#{}]'.format(dummy2)
    # Read the input molecule files
    mol1 = Chem.MolFromMolFile(mol1_path)
    mol2 = Chem.MolFromMolFile(mol2_path)
    try:
        dummy_idx1 = mol1.GetSubstructMatch(Chem.MolFromSmarts(form_dummy1))[0]
        dummy_idx2 = mol2.GetSubstructMatch(Chem.MolFromSmarts(form_dummy2))[0]
    except IndexError:
        dummy_idx1 = mol1.GetSubstructMatch(Chem.MolFromSmarts(form_dummy2))[0]
        dummy_idx2 = mol2.GetSubstructMatch(Chem.MolFromSmarts(form_dummy1))[0]
    adjacent_atom1 = mol1.GetAtomWithIdx(dummy_idx1).GetNeighbors()[0]
    adjacent_atom2 = mol2.GetAtomWithIdx(dummy_idx2).GetNeighbors()[0]
    # Combine the molecules
    combined_mol = Chem.CombineMols(mol1, mol2)
    # Add a bond between the atoms adjacent to the dummy atoms
    combined_mol_edit = Chem.EditableMol(combined_mol)
    combined_mol_edit.AddBond(adjacent_atom1.GetIdx(), len(mol1.GetAtoms()) + adjacent_atom2.GetIdx(), Chem.rdchem.BondType.SINGLE)
    # Remove the dummy atoms from the combined molecule
    combined_mol_edit.RemoveAtom(len(mol1.GetAtoms()) + dummy_idx2)
    combined_mol_edit.RemoveAtom(dummy_idx1)

    combined_mol = combined_mol_edit.GetMol()
    Chem.Kekulize(combined_mol)
    Chem.SanitizeMol(combined_mol)

    with open(output_file, 'w') as f:
        f.write(Chem.MolToMolBlock(combined_mol))

    return combined_mol

def cofmer_cleaner(directory):
    # Get a list of all files in the directory
    file_list = os.listdir(directory)
    
    # Find the highest integer in the Product_ filenames
    highest_int = -1
    for file in file_list:
        if file.startswith("Product_") and file.endswith(".mol"):
            file_num = int(file.split("_")[1].split(".")[0])
            if file_num > highest_int:
                highest_int = file_num
    
    # Open the highest numbered Product_ file and replace high atomic numbers with dummy atoms
    file_name = f"Product_{highest_int}.mol"
    file_path = os.path.join(directory, file_name)
    mol = Chem.MolFromMolFile(file_path)
    for atom in mol.GetAtoms():
        if atom.GetAtomicNum() > 89:
            atom.SetAtomicNum(0)
            atom.SetProp("_name", "*")

    # Save the modified molecule as COFmer.mol
    output_path = os.path.join(directory, "COFmer.mol")
    with open(output_path, 'w') as f:
        f.write(Chem.MolToMolBlock(mol))

    # Clean the COFmer.mol file
    for filename in os.listdir(directory):
        if filename == "COFmer.mol":
            file_path = os.path.join(directory, filename)
            with open(file_path, 'r') as f:
                mol_block = f.read()

            # Replace 'RDKit' with 'AK-CARES'
            mol_block = mol_block.replace('RDKit', 'AK-CARES')
            mol_block = mol_block.replace(' R ', ' * ')

            # Remove lines starting with V, A, or *
            mol_lines = mol_block.split('\n')
            cleaned_lines = [line for line in mol_lines if not line.startswith(('V', 'A', '*'))]
            cleaned_mol_block = '\n'.join(cleaned_lines)

            with open(file_path, 'w') as f:
                f.write(cleaned_mol_block)
               
                     
def run_cofmer_pipeline(assemblyModel, componentTypeNumber, precursor1, linkage1, input_dir, output_dir):
    SingleComponents = component_handler(assemblyModel, componentTypeNumber, [precursor1], [linkage1])
    component_mol_handler(SingleComponents, input_dir, output_dir)
    initiation_single_component = initial_product_instantiation(assemblyModel, SingleComponents)
    looping_single_components = looping_to_cofmer(assemblyModel, initiation_single_component, product_instantiation)
    #with open('data.json', 'w') as f:
    #    json.dump(looping_single_components, f, indent=4)
    product_mol_handler(looping_single_components)
    new_dir = r"C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\output_dir\\"
    cofmer_cleaner(new_dir)
    
