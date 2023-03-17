import csv

input_file = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\Precursor.csv'
expanded_output_file = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\Alg1_Output.csv'

# Function to create a nested dictionary from the input csv file that contains binding sites and cores describing precursors

def create_nested_dict(input_file):
    result_dict = {}
    with open(input_file, 'r', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)
        next(reader)
        for row in reader:
            if row[1] not in result_dict:
                result_dict[row[1]] = []
            result_dict[row[1]].append(row[0])

    result_list = [{'BS': bs, 'CORE': cores} for bs, cores in result_dict.items()]

    return result_list

# Function to find common cores between sets of cores grouped based on binding sites and update the precursor space
def find_common_cores(result_list, expanded_output_file):
    expanded_dict = {}
    
    # Loop through each outer dictionary in the result list
    for i, outer_dict in enumerate(result_list):
        outer_bs, outer_cores = outer_dict['BS'], set(outer_dict['CORE'])

        # Loop through each inner dictionary in the result list
        for j in range(i+1, len(result_list)):
            inner_dict = result_list[j]
            inner_bs, inner_cores = inner_dict['BS'], set(inner_dict['CORE'])
            common_cores = outer_cores.intersection(inner_cores)
            
             # If there are common cores, update the outer and inner core sets
            if common_cores:
                outer_cores.update(inner_cores)
                inner_cores.update(outer_cores)

        expanded_dict[outer_bs] = list(outer_cores)
    
    # Write the updated precursor space to the output file
    with open(expanded_output_file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['BS', 'CORE'])

        for core, bss in expanded_dict.items():
            for bs in bss:
                writer.writerow([bs, core])

# Create a nested dictionary from the input file
result_list = create_nested_dict(input_file)

# Find common cores and update the precursor space
find_common_cores(result_list, expanded_output_file)