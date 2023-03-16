import csv
import json

input_file = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\Precursor.csv'
output_file = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\temp\\alg1_result.json'
expanded_output_file = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Alg1_Output.csv'

def create_nested_dict(input_file, output_file):
    # Create an empty dictionary to store the results
    result_dict = {}

    # Open the input file and read its contents using utf-8 encoding
    with open(input_file, 'r', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)
        next(reader) # skip the header row
        for row in reader:
            core = row[0]
            bs = row[1]
            if bs not in result_dict:
                result_dict[bs] = []
            result_dict[bs].append(core)

    # Create a list of nested dictionaries pairing BS with the list of CORE values
    result_list = [{'BS': bs, 'CORE': cores} for bs, cores in result_dict.items()]
    print (result_list)

    # Write the result list to a JSON file with proper indentation
    with open(output_file, 'w') as outfile:
        json.dump(result_list, outfile, indent=4)

    # Print a success message
    #print('Results saved to ' + output_file)

    return result_list

def find_common_cores(result_list, expanded_output_file):
    # Create an empty set to store unique CORE values
    unique_cores = set()

    # Create a list to store statements about common CORE values
    common_cores_statements = []

    # Create a dictionary to store the expanded precursors
    expanded_dict = {}

    # Loop over each nested dictionary in the result list
    for i, outer_dict in enumerate(result_list):
        outer_bs = outer_dict['BS']
        outer_cores = set(outer_dict['CORE'])

        # Check for common CORE values with nested dictionaries later in the list
        for j in range(i+1, len(result_list)):
            inner_dict = result_list[j]
            inner_bs = inner_dict['BS']
            inner_cores = set(inner_dict['CORE'])
            common_cores = outer_cores.intersection(inner_cores)

            # If common CORE values are found, update the sets and add a statement to the list
            if common_cores:
                statement = f"BS values {outer_bs} and {inner_bs} share CORE value(s): {', '.join(common_cores)}"
                common_cores_statements.append(statement)
                outer_cores.update(inner_cores)
                inner_cores.update(outer_cores)

            # Add the unique CORE values to the set
            unique_cores.update(outer_cores)
            unique_cores.update(inner_cores)

        # Store the expanded values in the dictionary
        expanded_dict[outer_bs] = list(outer_cores)
        #print(expanded_dict)

    # Write the expanded dictionary to a CSV file
    with open(expanded_output_file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)

        # Write the header row
        writer.writerow(['BS', 'CORE'])

        # Loop through each key-value pair in the expanded dictionary
        for core, bss in expanded_dict.items():
            
            # Write a row for each CORE value
            for bs in bss:
                writer.writerow([bs, core])
                
    # Print a success message
    #print(f'Results saved to {expanded_output_file}')

result_list = create_nested_dict(input_file, output_file)
find_common_cores(result_list, expanded_output_file)