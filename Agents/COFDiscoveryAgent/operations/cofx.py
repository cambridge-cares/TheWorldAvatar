import json
import itertools
import os
import csv

def load_json(file_path):
    try:
        with open(file_path, 'r') as file:
            return json.load(file)
    except FileNotFoundError:
        print(f"Error: The file {file_path} does not exist.")
        return None

def save_json(data, file_path):
    with open(file_path, 'w') as file:
        json.dump(data, file, indent=4)

def linkage_binding_site_analysis(data):
    # Extract unique linkages with their binding site labels and LGBU
    linkage_info = {}
    for item in data:
        if 'Linkage' in item:
            linkage_uuid = item['Linkage']['LFR_UUID']
            linkage_lgbu = item['Linkage']['LGBU']
            binding_sites = item['Linkage'].get('hasBindingSite', [])
            # Store binding site labels as a set and also store the LGBU
            linkage_info[linkage_uuid] = {'LGBU': linkage_lgbu, 'BindingSiteLabels': {bs['Binding_Site_Label'] for bs in binding_sites}}
    
    # Find all precursors with matching binding site labels
    precursor_match = {uuid: {} for uuid in linkage_info}
    for item in data:
        if 'Precursor' in item:
            for precursor in item['Precursor']:
                precursor_uuid = precursor['Precursor_UUID']
                precursor_bs_labels = {bs['Binding_Site_Label'] for bs in precursor.get('hasBindingSite', [])}
                
                for linkage_uuid, linkage_details in linkage_info.items():
                    bs_labels = linkage_details['BindingSiteLabels']
                    matching_bs_labels = bs_labels.intersection(precursor_bs_labels)
                    for label in matching_bs_labels:
                        if label in precursor_match[linkage_uuid]:
                            if precursor_uuid not in precursor_match[linkage_uuid][label]:
                                precursor_match[linkage_uuid][label].append(precursor_uuid)
                        else:
                            precursor_match[linkage_uuid][label] = [precursor_uuid]

    # Formatting results for each linkage, including LGBU and binding site label
    result = []
    for linkage_uuid, bs_dict in precursor_match.items():
        linkage_details = []
        for bs_label, precursors in bs_dict.items():
            linkage_details.append({'Binding Site Label': bs_label, 'Precursors': list(set(precursors))})  # Ensure unique precursors
        result.append({
            'Linkage UUID': linkage_uuid,
            'LGBU': linkage_info[linkage_uuid]['LGBU'],
            'Details': linkage_details
        })

    return result

def find_assembly_models_and_precursors_by_gbu(data):
    # Find Assembly Models and precursors matching GBU labels
    lgbu_am_mapping = {}
    for item in data:
        if 'AssemblyModel' in item:
            assembly_model = item['AssemblyModel']
            am_uuid = assembly_model['AM_UUID']
            for lgbu in assembly_model.get('LGBUs', []):
                lgbu_label = lgbu['Label']
                if lgbu_label not in lgbu_am_mapping:
                    lgbu_am_mapping[lgbu_label] = []
                # Each GBU entry now includes a Precursors list
                gbu_details = [{
                    'Label': gbu['Label'],
                    'Number': gbu['Number'],
                    'Precursors': []  # Initialize an empty list for precursors
                } for gbu in assembly_model.get('GBUs', [])]
                model_entry = {
                    'AM_UUID': am_uuid,
                    'AM_Long_Formula': assembly_model.get('AM_Long_Formula', ''),
                    'AM_Short_Formula': assembly_model.get('AM_Short_Formula', ''),
                    'GBU_Details': gbu_details
                }
                lgbu_am_mapping[lgbu_label].append(model_entry)

    # Update GBU details with matching Precursors
    for item in data:
        if 'Precursor' in item:
            for precursor in item['Precursor']:
                precursor_uuid = precursor['Precursor_UUID']
                precursor_gbu_label = precursor['GBU']
                # Match precursors to specific GBU details
                for lgbu_label, models in lgbu_am_mapping.items():
                    for model in models:
                        for gbu_detail in model['GBU_Details']:
                            if gbu_detail['Label'] == precursor_gbu_label:
                                if precursor_uuid not in gbu_detail['Precursors']:
                                    gbu_detail['Precursors'].append(precursor_uuid)

    return lgbu_am_mapping

def prepare_combined_data(output_a, output_b):
    # Prepare linkage information from output_a.json
    linkage_info = {}
    for linkage in output_a:
        linkage_uuid = linkage["Linkage UUID"]
        lgbu = linkage["LGBU"]
        for detail in linkage["Details"]:
            binding_label = detail["Binding Site Label"]
            precursors = set(detail["Precursors"])
            linkage_info.setdefault((linkage_uuid, lgbu), {}).setdefault(binding_label, set()).update(precursors)

    # Prepare assembly model details from output_b.json
    assembly_info = {}
    for lgbu, models in output_b.items():
        for model in models:
            am_uuid = model["AM_UUID"]
            for gbu in model["GBU_Details"]:
                key = (lgbu, am_uuid, gbu["Label"])
                assembly_info.setdefault(key, set()).update(gbu["Precursors"])

    # Combine and evaluate overlaps, structured with grouped set combinations
    combined_info = {}
    set_combination_count = 1
    for (linkage_uuid, linkage_lgbu), linkage_details in linkage_info.items():
        for (assembly_lgbu, am_uuid, gbu_label), assembly_precursors in assembly_info.items():
            if linkage_lgbu == assembly_lgbu:  # Match based on LGBU
                combination_key = (linkage_uuid, am_uuid)
                if combination_key not in combined_info:
                    combined_info[combination_key] = {
                        "Sets_Combination_ID": f"Sets_Combination_{set_combination_count}",
                        "Linkage_UUID": linkage_uuid,
                        "Assembly_Model_UUID": am_uuid,
                        "Sets": {}
                    }
                    set_combination_count += 1
                set_index = len(combined_info[combination_key]["Sets"]) + 1
                for binding_label, link_precursors in linkage_details.items():
                    overlap = link_precursors.intersection(assembly_precursors)
                    if overlap:
                        set_key = f"Set {set_index}"
                        combined_info[combination_key]["Sets"][set_key] = {
                            "GBU_Label": gbu_label,
                            "Binding_Site_Label": binding_label,
                            "Precursors": list(overlap)
                        }
                        set_index += 1

    # Convert keys to final output format
    final_output = {}
    for key, value in combined_info.items():
        final_output[value["Sets_Combination_ID"]] = value
        del value["Sets_Combination_ID"]

    return final_output

def generate_precursor_combinations(data, known_cofs_file):
    known_cofs = load_json(known_cofs_file)

    if data is None or known_cofs is None:
        print("Error: One or more input files are missing.")
        return []

    known_combinations = set()
    for cof in known_cofs:
        am_uuid = cof['AssemblyModel']['AM_UUID']
        linkage_type = cof['Linkage']['LFR_UUID']
        precursor_pair_key = frozenset([p['Precursor_UUID'] for p in cof['Precursor']])
        known_combinations.add((am_uuid, linkage_type, precursor_pair_key))

    results = []
    set_combination_keys = [key for key in data if key.startswith('Sets_Combination_')]
    cof_counter = 1  # Simple counter to number each unique COF

    for key in set_combination_keys:
        combination = data[key]
        sets = combination['Sets']
        set_keys = list(sets.keys())
        combinations = []

        if len(set_keys) == 4:
            set_a_precursors = sets[set_keys[0]]['Precursors']
            set_b_precursors = sets[set_keys[3]]['Precursors']
            set_c_precursors = sets[set_keys[1]]['Precursors']
            set_d_precursors = sets[set_keys[2]]['Precursors']
            combinations += list(itertools.product(set_a_precursors, set_b_precursors))
            combinations += list(itertools.product(set_c_precursors, set_d_precursors))
        elif len(set_keys) == 3:
            set_a_precursors = sets[set_keys[0]]['Precursors']
            set_b_precursors = sets[set_keys[1]]['Precursors']
            set_c_precursors = sets[set_keys[2]]['Precursors']
            combinations += list(itertools.product(set_a_precursors, set_b_precursors))
            combinations += list(itertools.product(set_b_precursors, set_c_precursors))
            combinations += list(itertools.product(set_a_precursors, set_c_precursors))
        elif len(set_keys) == 2:
            set_a_precursors = sets[set_keys[0]]['Precursors']
            set_b_precursors = sets[set_keys[1]]['Precursors']
            combinations += list(itertools.product(set_a_precursors, set_b_precursors))
        elif len(set_keys) == 1:
            # No combinations are needed, each precursor is listed on its own
            set_a_precursors = sets[set_keys[0]]['Precursors']
            combinations += [(precursor,) for precursor in set_a_precursors]  # Use tuples for consistency

        # Extract UUIDs for linkage and assembly model
        linkage_uuid = combination['Linkage_UUID']
        assembly_model_uuid = combination['Assembly_Model_UUID']

        # Generate unique COFs based on combinations of precursors
        for pair in combinations:
            precursor_pair_key = frozenset(pair)
            combo = (assembly_model_uuid, linkage_uuid, precursor_pair_key)
            if combo not in known_combinations:
                results.append({
                    'COF': f"COF_{cof_counter}",
                    'Linkage': linkage_uuid,
                    'AssemblyModel': assembly_model_uuid,
                    'Complementary Precursor Pair': list(pair)
                })
                cof_counter += 1  # Increment for each unique COF generated

    return results

def check_no_overlap(library_data, known_cofs):
    known_combinations = set()
    for cof in known_cofs:
        am_uuid = cof['AssemblyModel']['AM_UUID']
        linkage_type = cof['Linkage']['LFR_UUID']
        precursor_pair_key = frozenset([p['Precursor_UUID'] for p in cof['Precursor']])
        known_combinations.add((am_uuid, linkage_type, precursor_pair_key))

    overlaps = []
    for entry in library_data:
        am_uuid = entry['AssemblyModel']
        linkage_type = entry['Linkage']
        precursor_pair_key = frozenset(entry['Complementary Precursor Pair'])
        combo = (am_uuid, linkage_type, precursor_pair_key)
        if combo in known_combinations:
            overlaps.append(combo)

    if overlaps:
        print("Overlap found:")
        for overlap in overlaps:
            print(overlap)
    else:
        print("No overlap found between new COFs and known COFs.")

def produce_cof_instance_csv(library_data, known_cofs, csv_output_file):
    assembly_model_counts = {}

    def count_assembly_models(data, label):
        for entry in data:
            am_uuid = entry['AssemblyModel'] if isinstance(entry['AssemblyModel'], str) else entry['AssemblyModel']['AM_UUID']
            if am_uuid not in assembly_model_counts:
                assembly_model_counts[am_uuid] = {'Number_of_Known_COF_Instances': 0, 'Number_of_New_COF_Instances': 0}
            assembly_model_counts[am_uuid][label] += 1

    count_assembly_models(known_cofs, 'Number_of_Known_COF_Instances')
    count_assembly_models(library_data, 'Number_of_New_COF_Instances')

    with open(csv_output_file, 'w', newline='') as csvfile:
        fieldnames = ['AssemblyModel_UUID', 'Number_of_Known_COF_Instances', 'Number_of_New_COF_Instances']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        writer.writeheader()
        for am_uuid, counts in assembly_model_counts.items():
            writer.writerow({
                'AssemblyModel_UUID': am_uuid,
                'Number_of_Known_COF_Instances': counts['Number_of_Known_COF_Instances'],
                'Number_of_New_COF_Instances': counts['Number_of_New_COF_Instances']
            })

    print(f"CSV file {csv_output_file} has been created.")

def produce_linkage_cof_instance_csv(library_data, known_cofs, csv_output_file):
    linkage_counts = {}

    def count_linkages(data, label):
        for entry in data:
            linkage_uuid = entry['Linkage'] if isinstance(entry['Linkage'], str) else entry['Linkage']['LFR_UUID']
            if linkage_uuid not in linkage_counts:
                linkage_counts[linkage_uuid] = {'Number_of_Known_COF_Instances': 0, 'Number_of_New_COF_Instances': 0}
            linkage_counts[linkage_uuid][label] += 1

    count_linkages(known_cofs, 'Number_of_Known_COF_Instances')
    count_linkages(library_data, 'Number_of_New_COF_Instances')

    with open(csv_output_file, 'w', newline='') as csvfile:
        fieldnames = ['Linkage_UUID', 'Number_of_Known_COF_Instances', 'Number_of_New_COF_Instances']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        writer.writeheader()
        for linkage_uuid, counts in linkage_counts.items():
            writer.writerow({
                'Linkage_UUID': linkage_uuid,
                'Number_of_Known_COF_Instances': counts['Number_of_Known_COF_Instances'],
                'Number_of_New_COF_Instances': counts['Number_of_New_COF_Instances']
            })

    print(f"CSV file {csv_output_file} has been created.")

# Combined execution
if __name__ == "__main__":
    file_path = 'cof_data/output_json/cofs.json'
    known_cofs_file = 'cof_data/output_json/cofs.json'  # Ensure this file path is correct
    final_output_file = 'cof_data/output_json/new_cofs.json'
    csv_output_file_assembly_model = 'cof_data/output_json/am_vs_cofs.csv'
    csv_output_file_linkage = 'cof_data/output_json/linkage_vs_cofs.csv'

    data = load_json(file_path)

    output_a = linkage_binding_site_analysis(data)
    output_b = find_assembly_models_and_precursors_by_gbu(data)

    prepared_data = prepare_combined_data(output_a, output_b)

    final_output_data = generate_precursor_combinations(prepared_data, known_cofs_file)
    save_json(final_output_data, final_output_file)

    known_cofs = load_json(known_cofs_file)
    check_no_overlap(final_output_data, known_cofs)
    produce_cof_instance_csv(final_output_data, known_cofs, csv_output_file_assembly_model)
    produce_linkage_cof_instance_csv(final_output_data, known_cofs, csv_output_file_linkage)
