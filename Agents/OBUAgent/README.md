# Covalent Organic Framework Precursor Space Exploration Algorithms

## Repository Overview

This repository is dedicated to providing a collection of algorithms specifically designed for exploring precursor spaces in the context of covalent organic frameworks (COFs). These algorithms aim to efficiently analyze and process the relationships between COF precursors and their associated properties, facilitating deeper insights and understanding of the underlying structures and patterns in the development of advanced COF materials.

## Usage and Applications

The algorithms in this repository can be used for a wide range of applications related to covalent organic frameworks, including but not limited to:

- Discovering novel precursors for the synthesis of COFs with desired properties
- Analyzing the relationships between COF precursors and their associated properties, namely binding sites, core units, and outer coordination number of the binding sites.

By employing these algorithms, researchers and practitioners working on covalent organic frameworks can gain a better understanding of the complex relationships within COF precursor spaces and make more informed decisions in the design and development of advanced materials.

## Workflow

To run the algorithms, follow these steps:

1. Define the file paths for the input and output files in the main.py file.

2. Run Algorithm 1 by calling alg1.create_nested_dict and alg1.find_common_cores functions to create a nested dictionary from the precursor file and find common cores respectively.

3. Run Algorithm 2 by calling alg2.read_bs_data, alg2.read_reactions_file, alg2.extract_values, alg2.read_reaction_file, alg2.create_singles_bs_core, alg2.create_doubles_paired, alg2.merge_sets, and alg2.write_csv functions to read the input files, extract the necessary data, process the data, merge the data, and write the output file.

4. Run enumeration by calling enumeration.count_values, enumeration.dict_to_csv, enumeration.merge_counts, enumeration.merge_csv_files, and enumeration.remove_duplicates functions to count the occurrences of each binding site in different input files, save the count dictionaries as CSV files, merge the count CSV files into a single enumeration output file, merge Alg1 and Alg2 output files, remove duplicate rows from the merged Alg1 and Alg2 output file, and save it as the union output file.

## License

This project is licensed under the MIT License.