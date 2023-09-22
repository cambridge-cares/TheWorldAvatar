import os
from rdkit import Chem
from copy import deepcopy
from rdkit.Chem.MolStandardize import rdMolStandardize
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

    components = {k: v for k, v in assemblyModel.items() if "Component" in k}
    matched_gbus = set()
    for precursor in precursors:
        gbu = precursor.get("GBU", "")
        if gbu in matched_gbus:
            continue
        precursor_info = {
            "ConstructingMol": precursor.get("ConstructingMol", ""),
            'Dentation': precursor.get("Dentation", ""),
            "BS": precursor.get("BS", []) 
        }
        for k, v in components.items():
            if v == gbu and k not in matched_gbus:
                components[k] = precursor_info["ConstructingMol"]
                components[k + " BS"] = precursor_info["BS"]
                matched_gbus.add(k)
                break  # Break out of the loop once a match is found

    for linkage in linkages:

        gbu = linkage.get("GBU", "")
        linkage_info = {
            "ConstructingMol": linkage.get("ConstructingMol", ""),
            'Dentation': linkage.get("Dentation", ""),
            "BS": linkage.get("BS", [])
        }
        for k, v in components.items():
            if v == gbu:
                components[k] = linkage_info["ConstructingMol"]
                components[k + " BS"] = linkage_info["BS"]
                break  # Break out of the loop once a match is found
    return components

def ordering_components(input_dict):
    #print(input_dict)
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
    #print(component_dict)
    dummies_start = 88
    output_dict = {}
    
    for component_name, component_data in component_dict.items():
        component_copies = component_data['Copies']
        bs_data_sorted = sorted(component_data['BS'], key=lambda x: x['bsIndex'])
        
        # Assign to all copies
        for i in range(1, component_copies + 1):
            first_copy_bs_data_list = []

            for bs_data in bs_data_sorted:
                bs_index = bs_data['bsIndex']
                bs_data_copy = {'bindingSite': bs_data['bindingSite'], 'Dentation': bs_data['Dentation'], 'Dummies': [], 'Complementaries': []}

                # Check if Dummies and Complementaries are empty
                if not bs_data['Dummies'] and not bs_data['Complementaries']:
                    for j in range(bs_index):
                        bs_data_copy['Dummies'].append(dummies_start)
                        dummies_start += 1

                        bs_data_copy['Complementaries'].append(dummies_start)
                        dummies_start += 1

                        # Check after each complementary number
                        if dummies_start > 100:
                            dummies_start = 88
                else:
                    bs_data_copy['Dummies'] = bs_data['Dummies']
                    bs_data_copy['Complementaries'] = bs_data['Complementaries']

                first_copy_bs_data_list.append(bs_data_copy)

            copy_name = f"{component_name}_Copy_{i}"
            input_file = component_data[component_name]
            output_file = f"{copy_name}.mol"
            output_dict[copy_name] = {'inputFile': input_file, 'outputFile': output_file, 'Linkage_BS': first_copy_bs_data_list}
    #print(output_dict)        
    return output_dict

def dummify_precursors(component_dict):
    dummies_start = 101
    output_dict = {}
    
    for component_name, component_data in component_dict.items():
        component_copies = component_data['Copies']
        bs_data_sorted = sorted(component_data['BS'], key=lambda x: x['bsIndex'])
        
        # Assign to all copies
        for i in range(1, component_copies + 1):
            first_copy_bs_data_list = []

            for bs_data in bs_data_sorted:
                bs_index = bs_data['bsIndex']
                bs_data_copy = {'bindingSite': bs_data['bindingSite'], 'Dentation': bs_data['Dentation'], 'Dummies': [], 'Complementaries': []}

                # Check if Dummies and Complementaries are empty
                if not bs_data['Dummies'] and not bs_data['Complementaries']:
                    for j in range(bs_index):
                        bs_data_copy['Dummies'].append(dummies_start)
                        dummies_start += 1

                        bs_data_copy['Complementaries'].append(dummies_start)
                        dummies_start += 1

                        # Check after each complementary number
                        if dummies_start > 116:
                            dummies_start = 101
                else:
                    bs_data_copy['Dummies'] = bs_data['Dummies']
                    bs_data_copy['Complementaries'] = bs_data['Complementaries']

                first_copy_bs_data_list.append(bs_data_copy)

            copy_name = f"{component_name}_Copy_{i}"
            input_file = component_data[component_name]
            output_file = f"{copy_name}.mol"
            output_dict[copy_name] = {'inputFile': input_file, 'outputFile': output_file, 'Precursor_BS': first_copy_bs_data_list}
    ##print(output_dict)        
    return output_dict

#----------------------COMPONENT ISTANCE MOLECULIZER-------------------------

def process_mol(input_file, dummies=None, complementaries=None):
    with open(input_file, 'r') as f:
        mol_block = f.read()
    mol = Chem.MolFromMolBlock(mol_block)

    if dummies or complementaries:
        mol = dummy_atom_replacer(mol, dummies, complementaries)
    
    return Chem.MolToMolBlock(mol)

def extract_dummies_complementaries(data):
    """Extracts dummies and complementaries separately, omitting dummy value 87."""
    d_list, c_list = [], []

    for d in data:
        dummies = d.get('Dummies', [])
        complementaries = d.get('Complementaries', [])
        
        # Filter out 87 from dummies
        dummies = [item for item in dummies if item != 87]

        d_list.extend(dummies)
        c_list.extend(complementaries)
    
    return d_list, c_list

def extract_dummies_complementariesXX(data):
    """Extracts dummies and complementaries separately."""
    d_list, c_list = [], []

    for d in data:
        dummies = d.get('Dummies', [])
        complementaries = d.get('Complementaries', [])
        d_list.extend(dummies)
        c_list.extend(complementaries)
    
    return d_list, c_list

def component_mol_handler(component_dict, input_dir, output_dir):

    for component_name, component_data in component_dict.items():
        #print(component_name)
        #print(component_data)
        input_file = os.path.join(input_dir, component_data['inputFile'])
        output_file = os.path.join(output_dir, component_data['outputFile'])

        dummies, complementaries = [], []
        #print(dummies)
        # Check if 'Linkage_BS' or 'Precursor_BS' exist and retrieve dummies and complementaries
        if 'Linkage_BS' in component_data:
            d, c = extract_dummies_complementaries(component_data['Linkage_BS'])
            dummies.extend(d)
            complementaries.extend(c)
            #print(dummies)

        if 'Precursor_BS' in component_data:
            d, c = extract_dummies_complementaries(component_data['Precursor_BS'])
            dummies.extend(d)
            complementaries.extend(c)

        mol_block = process_mol(input_file, dummies if dummies else None, complementaries if complementaries else None)
            
        with open(output_file, 'w') as f:
            f.write(mol_block)
        
        # Assuming component_mol_cleaner is a separate function you have
        component_mol_cleaner(output_dir)

def dummy_atom_replacer(mol, dummies, complementaries):
    dummy_atom_index, complementary_atom_index = 0, 0

    for atom in mol.GetAtoms():
        if atom.GetAtomicNum() == 0 and dummies:
            atom.SetAtomicNum(dummies[dummy_atom_index % len(dummies)])
            dummy_atom_index += 1
        elif atom.GetAtomicNum() == 118 and complementaries:
            atom.SetAtomicNum(complementaries[complementary_atom_index % len(complementaries)])
            complementary_atom_index += 1

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

def initial_product_instantiation(assembly_model, SingleComponent_original):
    SingleComponent = deepcopy(SingleComponent_original)
    
    product1 = assembly_model["ConstructionSteps"]["Product_1"]
    
    component1 = product1[0]
    component2 = product1[1]
    outputFile = "Product_1.mol"
    inputFile = [SingleComponent[component1]['outputFile'], SingleComponent[component2]['outputFile']]

    Precursor_BS = []
    Linkage_BS = []

    matching_dummies = []
    matching_complementaries = []
    if "Precursor_BS" in SingleComponent[component1]:
        
        Precursor_BS.extend(SingleComponent[component1]["Precursor_BS"])
    if "Linkage_BS" in SingleComponent[component2]:
        Linkage_BS.extend(SingleComponent[component2]["Linkage_BS"])
    
    if "Linkage_BS" in SingleComponent[component2] and "Precursor_BS" in SingleComponent[component1]:
        for bs1 in SingleComponent[component1]["Precursor_BS"]:
            for bs2 in SingleComponent[component2]["Linkage_BS"]:
                #print(bs2)
                #if bs1["bindingSite"] == bs2["bindingSite"] and bs1["Dummies"] and bs2["Dummies"]:
                #    matching_dummies = [bs1["Dummies"].pop(0), bs2["Dummies"].pop(0)]   
                if bs1["bindingSite"] == bs2["bindingSite"] and bs1["Dummies"] and bs2["Dummies"]:
                    matching_dummies = [bs1["Dummies"][0], bs2["Dummies"][0]]
                    bs1["Dummies"].remove(matching_dummies[0])
                    bs2["Dummies"].remove(matching_dummies[1])
                    if bs1["Dentation"] == 'Bidentate' or bs2["Dentation"] == 'Bidentate':
                        #print(bs1["Complementaries"])
                        #print(bs2["Complementaries"])
                        matching_complementaries = [bs1["Complementaries"][0], bs2["Complementaries"][0]]
                        bs1["Complementaries"].remove(matching_complementaries[0])
                        bs2["Complementaries"].remove(matching_complementaries[1])
                        break
                    else:
                        break

    # Filter out the elements where 'Dummies' list is empty
    Precursor_BS = [bs for bs in Precursor_BS if bs['Dummies']]
    Linkage_BS = [bs for bs in Linkage_BS if bs['Dummies']]

    Product_1 = {
        "outputFile": outputFile,
        "inputFile": inputFile,
        "BindingDummies": matching_dummies,
        "BindingComplementaries": matching_complementaries,
        "Precursor_BS": Precursor_BS,
        "Linkage_BS": Linkage_BS
    }
    
    SingleComponent_original['Product_1'] = Product_1

    return SingleComponent_original

def product_instantiation(assembly_model, CurrentComponent, product_name):
    working_component = deepcopy(CurrentComponent)
    product1 = assembly_model["ConstructionSteps"][product_name]
    Product_1 = None 
    
    component1 = product1[0]
    component2 = product1[1]
    matching_dummies = []
    matching_complementaries = [] # Added this line
    outputFile = product_name+".mol"
    inputFile = [CurrentComponent[component1]['outputFile'], CurrentComponent[component2]['outputFile']]
    if "Precursor_BS" in working_component[component1] and "Linkage_BS" in working_component[component2]:
        for bs1 in working_component[component1]["Precursor_BS"]:        
            for bs3 in working_component[component2]["Linkage_BS"]:      
                if bs1["bindingSite"] == bs3["bindingSite"]:
                    matching_dummies = [bs1["Dummies"][0], bs3["Dummies"][0]]
                    bs1["Dummies"].remove(matching_dummies[0])
                    bs3["Dummies"].remove(matching_dummies[1])                
                    #print(bs1)
                    #print(bs3)
                    # Added the following block for Complementaries
                    if bs1["Complementaries"] and bs3["Complementaries"]:
                        matching_complementaries = [bs1["Complementaries"][0], bs3["Complementaries"][0]]
                        bs1["Complementaries"].remove(matching_complementaries[0])
                        bs3["Complementaries"].remove(matching_complementaries[1])
                    else:
                        matching_complementaries = []

                    #matching_complementaries = [bs1["Complementaries"][0], bs3["Complementaries"][0]]
                    #bs1["Complementaries"].remove(matching_complementaries[0])
                    #bs3["Complementaries"].remove(matching_complementaries[1])

                    bs1_based = clean_dummies_list(working_component[component1]["Precursor_BS"])
                    Product_1 = {
                        "outputFile": outputFile, 
                        "inputFile": inputFile,
                        "BindingDummies": matching_dummies,
                        "BindingComplementaries": matching_complementaries, # Added this line
                        "Precursor_BS": working_component[component1]["Precursor_BS"]
                    }                    
                    bs2_based = clean_dummies_list(working_component[component1].get("Linkage_BS"))
                    bs3_based = clean_dummies_list(working_component[component2].get("Linkage_BS"))
                    if bs2_based is not None: 
                        if bs3_based is not None:               
                            bs23 = merging_dummies(bs2_based, bs3_based)
                            Product_1.setdefault("Linkage_BS", bs23)
                            CurrentComponent[product_name] = Product_1
                            return CurrentComponent
                    else: 
                        if bs2_based:                            
                            Product_1.setdefault("Linkage_BS", bs2_based)
                        if bs3_based:    
                            Product_1.setdefault("Linkage_BS", bs3_based)
                        CurrentComponent[product_name] = Product_1
                        return CurrentComponent

    elif "Linkage_BS" in working_component[component1] and "Precursor_BS" in working_component[component2]:
        for bs1 in working_component[component1]["Linkage_BS"]:        
            for bs3 in working_component[component2]["Precursor_BS"]:               
                if bs1["bindingSite"] == bs3["bindingSite"]:
                    matching_dummies = [bs1["Dummies"][0], bs3["Dummies"][0]]
                    bs1["Dummies"].remove(matching_dummies[0])
                    bs3["Dummies"].remove(matching_dummies[1])
                    #print(product_name)
                    #print(bs1)
                    #print(bs3)
                    # Added the following block for Complementaries
                    if bs1["Complementaries"] and bs3["Complementaries"]:
                        matching_complementaries = [bs1["Complementaries"][0], bs3["Complementaries"][0]]
                        bs1["Complementaries"].remove(matching_complementaries[0])
                        bs3["Complementaries"].remove(matching_complementaries[1])
                    else:
                        matching_complementaries = []

                    #matching_complementaries = [bs1["Complementaries"][0], bs3["Complementaries"][0]]
                    #bs1["Complementaries"].remove(matching_complementaries[0])
                    #bs3["Complementaries"].remove(matching_complementaries[1])

                    bs1_based = clean_dummies_list(working_component[component1]["Linkage_BS"])
                    Product_1 = {
                        "outputFile": outputFile, 
                        "inputFile": inputFile,
                        "BindingDummies": matching_dummies,
                        "BindingComplementaries": matching_complementaries, # Added this line
                    }
                    if bs1_based is not None: 
                        Product_1.setdefault("Linkage_BS", bs1_based)
                    bs2_based = clean_dummies_list(working_component[component1].get("Precursor_BS"))
                    bs3_based = clean_dummies_list(working_component[component2].get("Precursor_BS"))
                    if bs2_based is not None:
                        if bs3_based is not None:                    
                            bs23 = merging_dummies(bs2_based, bs3_based)
                            Product_1.setdefault("Precursor_BS", bs23)
                            CurrentComponent[product_name] = Product_1
                            return CurrentComponent
                    else: 
                        if bs2_based:                            
                            Product_1.setdefault("Precursor_BS", bs2_based)
                        if bs3_based:    
                            Product_1.setdefault("Precursor_BS", bs3_based)
                        CurrentComponent[product_name] = Product_1
                        return CurrentComponent

    return None

def clean_dummies_list(input_list):
    cleaned_list = [item for item in input_list if item.get('Dummies')]
    return cleaned_list if cleaned_list else None

def merging_dummies(*lists_of_dicts):
    merged_dicts = {}

    for dict_list in lists_of_dicts:
        for d in dict_list:
            binding_site = d['bindingSite']
            dummies = d['Dummies']
            complementaries = d['Complementaries']
            dentation = d['Dentation']

            if not dummies:  # If 'Dummies' list is empty, continue to the next iteration.
                continue

            if binding_site in merged_dicts:
                merged_dicts[binding_site]['Dummies'].extend(dummies)
                merged_dicts[binding_site]['Complementaries'].extend(complementaries)
            else:
                merged_dicts[binding_site] = {'bindingSite': binding_site, 'Dummies': dummies, 'Dentation':dentation,'Complementaries': complementaries}

    # Convert the merged_dicts back to a list
    result_list = list(merged_dicts.values())

    # Sort the 'Dummies' values in each dictionary
    for d in result_list:
        d['Dummies'] = sorted(d['Dummies'])

    return result_list

def looping_to_cofmer(assembly_model, initiation_single_component, product_instantiation):
    result_dict = {}
    construction_steps = assembly_model["ConstructionSteps"]
    current_component = initiation_single_component

    for product_name, components in construction_steps.items():
        if product_name == "Product_1":
            continue  # Skip Product_1 since it's already initialized
        if product_name == "COFmer":
            if components == "Product_1":
                return initiation_single_component  # Return initiation_single_component if COFmer value is Product_1
            continue  # Skip COFmer since it's already initialized
        
        temp_dict = product_instantiation(assembly_model, current_component, product_name)
        
        result_dict = {**result_dict, **temp_dict}

    return result_dict

#----------------------PRODUCT COMPONENT MOLECULIZER-------------------------

def product_mol_handler(input_dict, output_dir):
    output_list = []
   
    # Loop through all keys in the dictionary
    for key in input_dict.keys():
        # Check if the key contains the word "Product_"
        if "Product_" in key:
            # Extract the relevant information from the dictionary
            input_files = input_dict[key]["inputFile"]
            output_file = os.path.join(output_dir, input_dict[key]["outputFile"])
            
            binding_dummies = input_dict[key]["BindingDummies"]
            binding_complementaries = input_dict[key].get("BindingComplementaries", [])
            input_component_1 = os.path.join(output_dir, input_files[0])
            input_component_2 = os.path.join(output_dir, input_files[1])
            
            # Default values if lists are empty or do not have enough entries
            binding_dummy_1 = None
            binding_dummy_2 = None
            binding_complementary_1 = None
            binding_complementary_2 = None

            # Check lengths and update values if enough entries exist
            if len(binding_dummies) > 0:
                binding_dummy_1 = binding_dummies[0]
            if len(binding_dummies) > 1:
                binding_dummy_2 = binding_dummies[1]
            if len(binding_complementaries) > 0:
                binding_complementary_1 = binding_complementaries[0]
            if len(binding_complementaries) > 1:
                binding_complementary_2 = binding_complementaries[1]

            # Call the combiner function with the possibly updated values
            product_mol_combiner(input_component_1, binding_dummy_1, binding_complementary_1, 
                                input_component_2, binding_dummy_2, binding_complementary_2, output_file)

            # Call the combiner function with both dummies and complementaries
            #product_mol_combiner(input_component_1, binding_dummies[0], binding_complementaries[0], 
            #                     input_component_2, binding_dummies[1], binding_complementaries[1], output_file)
            
            # Create a new dictionary to store the information for this key
            new_dict = {
                "inputFile": input_files, 
                "outputFile": output_file, 
                "BindingDummies": binding_dummies,
                "BindingComplementaries": binding_complementaries
            }
            
            # Add the new dictionary to the output list
            output_list.append(new_dict)
    
    return output_list

def product_mol_combiner(mol1_path, dummy1, comp1, mol2_path, dummy2, comp2, output_file):
    form_dummy1= '[#{}]'.format(dummy1)
    form_dummy2= '[#{}]'.format(dummy2)
    mol1 = Chem.MolFromMolFile(mol1_path)
    mol2 = Chem.MolFromMolFile(mol2_path)

    dummy_idx1 = mol1.GetSubstructMatch(Chem.MolFromSmarts(form_dummy1))[0]
    dummy_idx2 = mol2.GetSubstructMatch(Chem.MolFromSmarts(form_dummy2))[0]
    adjacent_atom1 = mol1.GetAtomWithIdx(dummy_idx1).GetNeighbors()[0]
    adjacent_atom2 = mol2.GetAtomWithIdx(dummy_idx2).GetNeighbors()[0]

    atoms_to_remove = [dummy_idx1, len(mol1.GetAtoms()) + dummy_idx2]

    if comp1 and comp2:  # If comp1 and comp2 are not empty
        form_comp1= '[#{}]'.format(comp1)
        form_comp2= '[#{}]'.format(comp2)
        comp_idx1 = mol1.GetSubstructMatch(Chem.MolFromSmarts(form_comp1))[0]
        comp_idx2 = mol2.GetSubstructMatch(Chem.MolFromSmarts(form_comp2))[0]
        adjacent_atom3 = mol1.GetAtomWithIdx(comp_idx1).GetNeighbors()[0]
        adjacent_atom4 = mol2.GetAtomWithIdx(comp_idx2).GetNeighbors()[0]

        atoms_to_remove.extend([comp_idx1, len(mol1.GetAtoms()) + comp_idx2])

    combined_mol = Chem.CombineMols(mol1, mol2)

    combined_mol_edit = Chem.EditableMol(combined_mol)
    combined_mol_edit.AddBond(adjacent_atom1.GetIdx(), len(mol1.GetAtoms()) + adjacent_atom2.GetIdx(), Chem.rdchem.BondType.SINGLE)

    if comp1 and comp2:
        combined_mol_edit.AddBond(adjacent_atom3.GetIdx(), len(mol1.GetAtoms()) + adjacent_atom4.GetIdx(), Chem.rdchem.BondType.SINGLE)

    # Sort the indices of atoms to be removed in descending order
    atoms_to_remove.sort(reverse=True)

    for atom_idx in atoms_to_remove:
        combined_mol_edit.RemoveAtom(atom_idx)

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
    highest_int = 1
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
    if "COFmer.mol" in os.listdir(directory):
        file_path = os.path.join(directory, "COFmer.mol")
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
            
    # Normalize the cleaned molecule
    mol = Chem.MolFromMolFile(output_path)
    normalizer = rdMolStandardize.Normalizer()
    mol = normalizer.normalize(mol)

    # Save the normalized molecule back as COFmer.mol
    with open(output_path, 'w') as f:
        f.write(Chem.MolToMolBlock(mol))

def run_cofmer_pipeline(assemblyModel, componentTypeNumber, precursors, linkages, input_dir, output_dir):
    if not isinstance(precursors, list):
        precursors = [precursors]
    if not isinstance(linkages, list):
        linkages = [linkages]

    SingleComponents = component_handler(assemblyModel, componentTypeNumber, precursors, linkages)
    #print(SingleComponents)
    component_mol_handler(SingleComponents, input_dir, output_dir)
    initiation_single_component = initial_product_instantiation(assemblyModel, SingleComponents)
    #print(initiation_single_component)
    looping_single_components = looping_to_cofmer(assemblyModel, initiation_single_component, product_instantiation)
 #  print(looping_single_components)
    product_mol_handler(looping_single_components, output_dir)
    cofmer_cleaner(output_dir)

