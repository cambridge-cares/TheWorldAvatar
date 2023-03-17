import csv

bs_file_path =          r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\BS.csv'
reaction_file_path =    r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\Reactions.csv'
precursors_file_path =  r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\Precursor.csv'
output_file =           r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Alg2_Output.csv'

def read_bs_data(bs_file_path):
    with open(bs_file_path, 'r') as f:
        reader = csv.reader(f)
        next(reader)  # skip header row
        bs_data = {row[0]: int(row[1]) for row in reader}
    return bs_data

def read_reactions_file(reaction_file_path):
    doubles = {}
    singles = {}
    with open(reaction_file_path, 'r') as f:
        reader = csv.reader(f)
        next(reader)  # skip header row
        for row in reader:
            lfr = row[0]
            bs1 = row[1]
            bs2 = row[2]
            if bs2 == 'None':
                if lfr not in singles:
                    singles[lfr] = []
                singles[lfr].append(bs1)
            else:
                if lfr not in doubles:
                    doubles[lfr] = []
                doubles[lfr].append([bs1, bs2])
    return doubles, singles


def extract_values(input_dict):
    values_list = []
    for values in input_dict.values():
        values_list.extend(values)
    return values_list

def read_reaction_file(bs_data, bs_file_path):
    pairs = []
    not_pairs = []

    with open(bs_file_path, 'r', encoding='utf-8-sig') as g:
        reader2 = csv.reader(g)
        next(reader2) # skip header row
        bs_data_dict = {row[0]: row[1] for row in reader2}

    # Loop through each bs pair in the input data
    for bs_pair in bs_data:
        bs1_unsorted, bs2_unsorted = bs_pair
        ocn1 = bs_data_dict.get(bs1_unsorted)
        ocn2 = bs_data_dict.get(bs2_unsorted)

        if ocn1 == ocn2:
            pairs.append([bs1_unsorted, bs2_unsorted])
        else:
            not_pairs.append([bs1_unsorted, bs2_unsorted])

    return pairs, not_pairs

def create_singles_bs_core(values_list, precursor_file_path):
    singles_bs_core = {}
    
    for bs in values_list:
        cores_set = set()
        with open(precursor_file_path, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                bs_precursors = row['BS']
                core = row['CORE']
                if bs == bs_precursors:
                    cores_set.add(core)
        singles_bs_core[bs] = cores_set
    return singles_bs_core

def create_doubles_paired(values_list, precursor_file_path):
    doubles_paired = {}
    
    for bs in values_list:
        bs1 = bs[0]
        bs2 = bs[1]
        if bs1 not in doubles_paired:
            doubles_paired[bs1] = set()
        if bs2 not in doubles_paired:
            doubles_paired[bs2] = set()
        with open(precursor_file_path, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                bs_precursors = row['BS']
                core = row['CORE']
                if bs1 == bs_precursors:
                    doubles_paired[bs1].add(core)
                    doubles_paired[bs2].add(core)
                if bs2 == bs_precursors:
                    doubles_paired[bs1].add(core)
                    doubles_paired[bs2].add(core)
    return doubles_paired

def create_doubles_unpaired(values_list, precursor_file_path):
    doubles_unpaired = {}
    
    for bs in values_list:
        bs1 = bs[0]
        bs2 = bs[1]
        if bs1 not in doubles_unpaired:
            doubles_unpaired[bs1] = set()
        if bs2 not in doubles_unpaired:
            doubles_unpaired[bs2] = set()
        with open(precursor_file_path, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                bs_precursors = row['BS']
                core = row['CORE']
                if bs1 == bs_precursors:
                    doubles_unpaired[bs1].add(core)
                if bs2 == bs_precursors:
                    doubles_unpaired[bs2].add(core)
    return doubles_unpaired

def merge_sets(dict_list):
    merged_dict = {}
    for d in dict_list:
        for k, v in d.items():
            if k in merged_dict:
                merged_dict[k].update(v)
            else:
                merged_dict[k] = v
    return merged_dict

def write_csv(data, filename):
    with open(filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        # Write the header row
        writer.writerow(['BS', 'CORE'])
        # Write the data rows
        for group, plates in data.items():
            for plate in plates:
                writer.writerow([group, plate])

            
bs_data = read_bs_data(bs_file_path)

doubles, singles = read_reactions_file(reaction_file_path)
nested_dict_1 = extract_values(singles)
nested_dict_2 = extract_values(doubles)

pairs, not_pairs =  read_reaction_file(nested_dict_2, bs_file_path)

bs_core_1 = create_singles_bs_core(nested_dict_1, precursors_file_path)
bs_core_2p = create_doubles_paired(pairs, precursors_file_path)
bs_core_2u = create_doubles_paired(not_pairs, precursors_file_path)

bs_cores_all = [bs_core_1, bs_core_2p, bs_core_2u]

merged_cores = merge_sets(bs_cores_all)
write_csv(merged_cores, output_file)