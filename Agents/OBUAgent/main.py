from Algorithms import alg1, alg2, enumeration, create_precursors
from rdkit import RDLogger
RDLogger.DisableLog('rdApp.*')

# Define file paths
INPUT_PATH = 'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\'
OUTPUT_PATH = 'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\'
TEMP_PATH = OUTPUT_PATH + 'temp\\'
PRECURSOR_FILE = INPUT_PATH + 'Precursor.csv'
REACTION_FILE = INPUT_PATH + 'Reactions.csv'
BS_FILE = INPUT_PATH + 'BS.csv'
ALG1_OUTPUT_FILE = OUTPUT_PATH + 'Alg1_Output.csv'
ALG2_OUTPUT_FILE = OUTPUT_PATH + 'Alg2_Output.csv'
ALG1U2_OUTPUT_FILE = OUTPUT_PATH + 'UnionAlg1_2_Output.csv'
ENUM_OUTPUT_FILE = OUTPUT_PATH + 'Enumeration.csv'
BASE_ENUM_FILE = TEMP_PATH + 'base_enum.csv'
ALG1_ENUM_FILE = TEMP_PATH + 'alg1_enum.csv'
ALG2_ENUM_FILE = TEMP_PATH + 'alg2_enum.csv'
UNION_ENUM_FILE = TEMP_PATH + 'union_enum.csv'
MERGED_ALG1_2_FILE = TEMP_PATH + 'merged_alg1_2.csv'

# Run Algorithm 1
result_list = alg1.create_nested_dict(PRECURSOR_FILE)
alg1.find_common_cores(result_list, ALG1_OUTPUT_FILE)

# Run Algorithm 2
bs_data = alg2.read_bs_data(BS_FILE)
doubles, singles = alg2.read_reactions_file(REACTION_FILE)
nested_dict_1 = alg2.extract_values(singles)
nested_dict_2 = alg2.extract_values(doubles)
pairs, not_pairs = alg2.read_reaction_file(nested_dict_2, BS_FILE)
bs_core_1 = alg2.create_singles_bs_core(nested_dict_1, PRECURSOR_FILE)
bs_core_2p = alg2.create_doubles_paired(pairs, PRECURSOR_FILE)
bs_core_2u = alg2.create_doubles_unpaired(not_pairs, PRECURSOR_FILE)
bs_cores_all = [bs_core_1, bs_core_2p, bs_core_2u]
merged_cores = alg2.merge_sets(bs_cores_all)
alg2.write_csv(merged_cores, ALG2_OUTPUT_FILE)

# Run enumeration
base_count = enumeration.count_values(PRECURSOR_FILE)
alg1_count = enumeration.count_values(ALG1_OUTPUT_FILE)
alg2_count = enumeration.count_values(ALG2_OUTPUT_FILE)
union_dict = enumeration.count_values(ALG1U2_OUTPUT_FILE)
enumeration.dict_to_csv(base_count, BASE_ENUM_FILE)
enumeration.dict_to_csv(alg1_count, ALG1_ENUM_FILE)
enumeration.dict_to_csv(alg2_count, ALG2_ENUM_FILE)
enumeration.dict_to_csv(union_dict, UNION_ENUM_FILE)
enumeration.merge_counts(BASE_ENUM_FILE, ALG1_ENUM_FILE, ALG2_ENUM_FILE, UNION_ENUM_FILE, ENUM_OUTPUT_FILE)
enumeration.merge_csv_files(ALG1_OUTPUT_FILE, ALG2_OUTPUT_FILE, MERGED_ALG1_2_FILE)
enumeration.remove_duplicates(MERGED_ALG1_2_FILE, ALG1U2_OUTPUT_FILE)

# Run precursor creation
create_precursors.main()
print('Workflow Compleated')
