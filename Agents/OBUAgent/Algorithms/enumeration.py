import csv

# Define file paths
input_file = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\Precursor.csv'
output_alg1 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Alg1_Output.csv'
output_alg2 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Alg2_Output.csv'
output_union = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\UnionAlg1_2_Output.csv'
enumeration_output = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Enumeration.csv'
temp_dir = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\temp\\'

# Function to count the occurrences of each binding site in the input file
def count_values(file_path):
    counts = {}
    with open(file_path, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            bs, core = row['BS'], row['CORE']
            counts[bs] = counts.get(bs, 0) + 1
    return counts

# Function to write the count dictionary to a CSV file
def dict_to_csv(data, output_file):
    with open(output_file, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['Group', 'Count'])
        for key, value in data.items():
            writer.writerow([key, value])

# Function to merge two CSV files
def merge_csv_files(file1_path, file2_path, output_path):
    with open(file1_path, 'r') as f1, open(file2_path, 'r') as f2:
        reader1, reader2 = csv.DictReader(f1), csv.DictReader(f2)
        headers = reader1.fieldnames + [col for col in reader2.fieldnames if col not in reader1.fieldnames]
        rows = [row1 | {col: '' for col in reader2.fieldnames if col not in reader1.fieldnames} for row1 in reader1] + \
               [row2 | {col: '' for col in reader1.fieldnames if col not in reader2.fieldnames} for row2 in reader2]
    with open(output_path, 'w', newline='') as f_out:
        writer = csv.DictWriter(f_out, fieldnames=headers)
        writer.writeheader()
        writer.writerows(rows)

    print(f'Merged file saved to {output_path}')

# Function to remove duplicate rows from a CSV file
def remove_duplicates(input_file, output_file):
    unique_rows = set()
    with open(input_file, 'r') as file:
        reader = csv.reader(file)
        next(reader)
        for row in reader:
            unique_rows.add(tuple(row))
    with open(output_file, 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['BS', 'CORE'])
        writer.writerows(unique_rows)
    print(f'Duplicate rows removed. Output file created at {output_file}')

# Function to merge the count files into a single CSV file
def merge_counts(base_path, alg1_path, alg2_path, union_path, output_path):
    base_counts = {k: int(v) for k, v in csv.reader(open(base_path)) if not k.startswith('Group')}
    alg1_counts = {k: int(v) for k, v in csv.reader(open(alg1_path)) if not k.startswith('Group')}
    alg2_counts = {k: int(v) for k, v in csv.reader(open(alg2_path)) if not k.startswith('Group')}
    union_counts = {k: int(v) for k, v in csv.reader(open(union_path)) if not k.startswith('Group')}
    all_counts = {k: {'Base': base_counts.get(k, 0),'Alg1': alg1_counts.get(k, 0), 'Alg2': alg2_counts.get(k, 0), 'Union': union_counts.get(k, 0)} for k in set().union(base_counts, alg1_counts, alg2_counts, union_counts)}

    try:
        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['BS', 'Base', 'Alg1', 'Alg2', 'Union'])
            for key, counts in all_counts.items():
                writer.writerow([key, counts['Base'], counts['Alg1'], counts['Alg2'], counts['Union']])
        print(f'Merged counts saved to {output_path}')
    except Exception as e:
        print(f'Error occurred while writing merged counts to {output_path}: {e}')

# Count the occurrences of each binding site in different input files    
base_count = count_values(input_file)
alg1_count = count_values(output_alg1)
alg2_count = count_values(output_alg2)

# Create a temporary file path for the merged Alg1 and Alg2 CSV files
merged_alg1_2 = temp_dir + 'merged_alg1_2.csv'

# Count the occurrences of each binding site in the union output file
union_dict = count_values(output_union)

# Save the count dictionaries as CSV files
dict_to_csv(base_count, temp_dir + 'base_enum.csv')
dict_to_csv(alg1_count, temp_dir + 'alg1_enum.csv')
dict_to_csv(alg2_count, temp_dir + 'alg2_enum.csv')
dict_to_csv(union_dict, temp_dir + 'union_enum.csv')

# Merge the count CSV files into a single enumeration output file
merge_counts(temp_dir + 'base_enum.csv', temp_dir + 'alg1_enum.csv', temp_dir + 'alg2_enum.csv', temp_dir + 'union_enum.csv', enumeration_output)

# Merge Alg1 and Alg2 output files
merge_csv_files(output_alg1, output_alg2, merged_alg1_2)

# Remove duplicate rows from the merged Alg1 and Alg2 output file and save it as the union output file
remove_duplicates(merged_alg1_2, output_union)
