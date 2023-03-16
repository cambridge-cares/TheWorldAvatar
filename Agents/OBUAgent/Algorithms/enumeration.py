import csv

input_file = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\Precursor.csv'
output_alg1 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Alg1_Output.csv'
output_alg2 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Alg2_Output.csv'
output_union = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\UnionAlg1_2_Output.csv'
enumeration_output = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Enumeration.csv'
enumeration_base = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\temp\\base_enum.csv'
enumeration_alg1 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\temp\\alg1_enum.csv'
enumeration_alg2 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\temp\\alg2_enum.csv'
merged_alg1_2 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\temp\\merged_alg1_2.csv'
enumeration_union = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\temp\\union_enum.csv'

def count_values(file_path):
    counts = {}
    with open(file_path, 'r') as f:
        reader = csv.reader(f)
        next(reader)  # skip header row
        for row in reader:
            key = row[0]
            value = row[1]
            if key in counts:
                counts[key] += 1
            else:
                counts[key] = 1
    return counts

def dict_to_csv(data, output_file):
    with open(output_file, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['Group', 'Count'])
        for key, value in data.items():
            writer.writerow([key, value])

def count_values(file_path):
    counts = {}
    with open(file_path, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            bs = row['BS']
            core = row['CORE']
            if bs in counts:
                if core not in counts[bs]:
                    counts[bs].append(core)
            else:
                counts[bs] = [core]
    return {k: len(v) for k, v in counts.items()}
    
def merge_csv_files(file1_path, file2_path, output_path):
    # Open the two input files and read them as dictionaries
    with open(file1_path, 'r') as f1, open(file2_path, 'r') as f2:
        reader1 = csv.DictReader(f1)
        reader2 = csv.DictReader(f2)
        
        # Combine the headers from both input files
        headers = reader1.fieldnames + [col for col in reader2.fieldnames if col not in reader1.fieldnames]
        
        # Combine the rows from both input files
        rows = []
        for row1 in reader1:
            row = {col: row1[col] for col in reader1.fieldnames}
            for col in reader2.fieldnames:
                if col not in reader1.fieldnames:
                    row[col] = ''
            rows.append(row)
        for row2 in reader2:
            row = {col: row2[col] for col in reader2.fieldnames}
            for col in reader1.fieldnames:
                if col not in reader2.fieldnames:
                    row[col] = ''
            rows.append(row)
            
    # Write the merged rows to the output file
    with open(output_path, 'w', newline='') as f_out:
        writer = csv.DictWriter(f_out, fieldnames=headers)
        writer.writeheader()
        writer.writerows(rows)

    print('Merged file saved to', output_path)
    
def remove_duplicates(input_file, output_file):
    # Create a set to hold unique rows
    unique_rows = set()

    # Open the input file and loop over its rows
    with open(input_file, 'r') as file:
        reader = csv.reader(file)
        next(reader) # skip the first row
        for row in reader:
            # Convert the row to a tuple and add it to the set
            row_tuple = tuple(row)
            unique_rows.add(row_tuple)

    # Open the output file and write the unique rows
    with open(output_file, 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['BS', 'CORE']) # add column names
        for row in unique_rows:
            writer.writerow(row)

    print('Duplicate rows removed. Output file created.')

def merge_counts(base_path, alg1_path, alg2_path, union_path, output_path):
    # Read the counts from the input files
    base_counts = {}
    with open(base_path, 'r') as f:
        reader = csv.reader(f)
        next(reader) # skip header row
        for row in reader:
            base_counts[row[0]] = int(row[1])

    alg1_counts = {}
    with open(alg1_path, 'r') as f:
        reader = csv.reader(f)
        next(reader) # skip header row
        for row in reader:
            alg1_counts[row[0]] = int(row[1])

    alg2_counts = {}
    with open(alg2_path, 'r') as f:
        reader = csv.reader(f)
        next(reader) # skip header row
        for row in reader:
            alg2_counts[row[0]] = int(row[1])

    union_counts = {}
    with open(union_path, 'r') as f:
        reader = csv.reader(f)
        next(reader) # skip header row
        for row in reader:
            union_counts[row[0]] = int(row[1])

    # Combine the counts into a single dictionary
    all_counts = {}
    for key in set(base_counts.keys()) | set(alg1_counts.keys()) | set(alg2_counts.keys()) | set(union_counts.keys()):
        all_counts[key] = {
            'Base': base_counts.get(key, 0),
            'Alg1': alg1_counts.get(key, 0),
            'Alg2': alg2_counts.get(key, 0),
            'Union': union_counts.get(key, 0)
        }

    # Write the merged counts to the output file
    with open(output_path, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['BS', 'Base', 'Alg1', 'Alg2', 'Union'])
        for key, counts in all_counts.items():
            writer.writerow([key, counts['Base'], counts['Alg1'], counts['Alg2'], counts['Union']])

    print('Merged counts saved to', output_path)
    
    
base_count = count_values(input_file)
alg1_count = count_values(output_alg1)
alg2_count = count_values(output_alg2)
merge_csv_files(output_alg1, output_alg2, merged_alg1_2)
remove_duplicates(merged_alg1_2, output_union)
union_dict = count_values(output_union)

dict_to_csv(base_count, enumeration_base)
dict_to_csv(alg1_count, enumeration_alg1)
dict_to_csv(alg2_count, enumeration_alg2)
dict_to_csv(union_dict, enumeration_union)
merge_counts(enumeration_base, enumeration_alg1, enumeration_alg2, enumeration_union, enumeration_output)