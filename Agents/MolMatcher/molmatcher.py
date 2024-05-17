import numpy as np
import pandas as pd
import networkx as nx
import os
import logging

# Configure logging
logging.basicConfig(level=logging.DEBUG, format='%(asctime)s - %(levelname)s - %(message)s')

def parse_molecular_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    num_atoms = int(lines[0].strip())
    atoms = [(line.split()[0], np.array(list(map(float, line.split()[1:]))))
             for line in lines[2:2+num_atoms]]
    logging.debug(f"Parsed {len(atoms)} atoms from {file_path}")
    return atoms, lines[:2]

def parse_mol_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    num_atoms = int(lines[3][0:3].strip())
    atoms = [(line.split()[3], np.array(list(map(float, line.split()[:3]))))
             for line in lines[4:4+num_atoms]]
    num_bonds = int(lines[3][3:6].strip())
    bonds = [(int(line.split()[0]) - 1, int(line.split()[1]) - 1, int(line.split()[2]))
             for line in lines[4+num_atoms:4+num_atoms+num_bonds]]
    logging.debug(f"Parsed {len(atoms)} atoms and {len(bonds)} bonds from {file_path}")
    return atoms, bonds

def distance(p1, p2):
    return np.linalg.norm(p1 - p2)

def find_connections(atoms, threshold_non_h=2.0, threshold_h=1.2):
    bonds = []
    for i, (atom1, pos1) in enumerate(atoms):
        for j, (atom2, pos2) in enumerate(atoms[i+1:], start=i+1):
            dist = distance(pos1, pos2)
            threshold = threshold_h if 'H' in (atom1, atom2) else threshold_non_h
            if dist <= threshold:
                bonds.append((i, j))
    logging.debug(f"Found {len(bonds)} bonds")
    return bonds

def create_graph(atoms, bonds, include_bond_order=False):
    G = nx.Graph()
    for i, (atom, _) in enumerate(atoms):
        G.add_node(i, element=atom)
    if include_bond_order:
        for bond in bonds:
            G.add_edge(bond[0], bond[1], order=bond[2])
    else:
        G.add_edges_from((bond[0], bond[1]) for bond in bonds)
    logging.debug(f"Created graph with {len(G.nodes)} nodes and {len(G.edges)} edges")
    return G

def check_subgraph_isomorphism(graph1, graph2, node1, node2, mapping, used_nodes):
    mapping[node1] = node2
    used_nodes.add(node2)
    for neighbor in graph1.neighbors(node1):
        if neighbor in mapping:
            continue
        match_found = False
        for potential_match in graph2.neighbors(node2):
            if potential_match in used_nodes:
                continue
            if (graph1.nodes[neighbor]['element'] == graph2.nodes[potential_match]['element'] and
                    graph1.degree[neighbor] <= graph2.degree[potential_match]):
                if check_subgraph_isomorphism(graph1, graph2, neighbor, potential_match, mapping, used_nodes):
                    match_found = True
                    break
        if not match_found:
            return False
    return True

def find_subgraph_isomorphisms(graph1, graph2):
    sorted_nodes = sorted(graph1.nodes, key=lambda n: graph1.degree[n])
    isomorphisms = []
    for node1 in sorted_nodes:
        for node2 in graph2.nodes:
            if (graph1.nodes[node1]['element'] == graph2.nodes[node2]['element'] and
                    graph1.degree[node1] <= graph2.degree[node2]):
                mapping = {}
                used_nodes = set()
                if check_subgraph_isomorphism(graph1, graph2, node1, node2, mapping, used_nodes):
                    isomorphisms.append(mapping)
    return isomorphisms

def write_xyz_file(file_path, header, atoms, bonds):
    with open(file_path, 'w') as file:
        file.write(header[0])
        file.write(header[1])
        for atom, pos in atoms:
            file.write(f"{atom} {' '.join(map(str, pos))}\n")
        for bond in bonds:
            file.write(f"{bond[0]+1} {bond[1]+1} {bond[2]}\n")

# Define the base directory for data
base_dir = 'data'

# Define the directories for input and output files
input_csv = os.path.join(base_dir, 'Precursors_inp.csv')
mol_dir = os.path.join(base_dir, 'mol')
precursor_378_dir = os.path.join(base_dir, 'Precursors_378')
output_dir = os.path.join(base_dir, 'A_NEW_XYZ')
check_csv = os.path.join(base_dir, 'precursors_check.csv')
valid_csv = os.path.join(base_dir, 'valid_mappings.csv')

# Step 1: Read the Precursors_inp.csv file
precursors_df = pd.read_csv(input_csv)

# Step 2: Find all unique precursor molecules
unique_precursors = precursors_df['Precursor'].unique()

# Initialize a list to store the results
results = []

# Step 3 and 4: Check for .mol and .xyz files, and prepare the results
for precursor in unique_precursors:
    precursor_base = precursor.split('.')[0].strip()  # Extract the base name (before the extension) and remove any whitespace
    
    mol_file = os.path.join(mol_dir, f'{precursor_base}.mol')
    xyz_file = os.path.join(precursor_378_dir, f'{precursor_base}.xyz')
    
    mol_present = os.path.isfile(mol_file)
    xyz_present = os.path.isfile(xyz_file)
    
    results.append({
        'Precursor': precursor,
        'mol_present': mol_present,
        'xyz_present': xyz_present,
        'Valid_Mapping': False  # Initialize with False, will be updated later
    })

# Convert the results list to a DataFrame
results_df = pd.DataFrame(results)

# Save the results to a CSV file
results_df.to_csv(check_csv, index=False)

logging.info(f"Check completed and results saved to '{check_csv}'")

# Read the precursors_check.csv file
df = pd.read_csv(check_csv)

# Filter the DataFrame to include only rows where both files are present
df_valid = df[(df['mol_present'] == True) & (df['xyz_present'] == True)]

for index, row in df_valid.iterrows():
    precursor = row['Precursor']
    precursor_base = precursor.split('.')[0].strip()
    
    file_path_xyz = os.path.join(precursor_378_dir, f'{precursor_base}.xyz')
    file_path_mol = os.path.join(mol_dir, f'{precursor_base}.mol')
    
    logging.debug(f"Processing {precursor}: .xyz file = {file_path_xyz}, .mol file = {file_path_mol}")
    
    atoms_xyz, header_xyz = parse_molecular_file(file_path_xyz)
    bonds_xyz = find_connections(atoms_xyz)
    graph_xyz = create_graph(atoms_xyz, bonds_xyz)
    
    atoms_mol, bonds_mol = parse_mol_file(file_path_mol)
    graph_mol = create_graph(atoms_mol, bonds_mol, include_bond_order=True)
    
    # Logging the nodes to verify 'element' attribute
    logging.debug(f".xyz graph nodes: {graph_xyz.nodes(data=True)}")
    logging.debug(f".mol graph nodes: {graph_mol.nodes(data=True)}")
    
    try:
        subgraph_isomorphisms = find_subgraph_isomorphisms(graph_mol, graph_xyz)
        if subgraph_isomorphisms:
            results_df.loc[index, 'Valid_Mapping'] = True
            first_mapping = subgraph_isomorphisms[0]
            mapped_bonds = []
            for (mol_atom1, mol_atom2, bond_order) in bonds_mol:
                if mol_atom1 in first_mapping and mol_atom2 in first_mapping:
                    xyz_atom1 = first_mapping[mol_atom1]
                    xyz_atom2 = first_mapping[mol_atom2]
                    mapped_bonds.append((xyz_atom1, xyz_atom2, bond_order))
                    logging.debug(f"Bond: {xyz_atom1+1} - {xyz_atom2+1} with bond order {bond_order}")
            
            # Add hydrogen and other missing atom bonds from the original .xyz file
            for bond in bonds_xyz:
                if not any((bond[0] == b[0] and bond[1] == b[1]) or (bond[0] == b[1] and bond[1] == b[0]) for b in mapped_bonds):
                    mapped_bonds.append((bond[0], bond[1], 1))  # Assuming single bond for connections found in .xyz
            
            new_xyz_file = os.path.join(output_dir, f'{precursor_base}_mapped.xyz')
            os.makedirs(os.path.dirname(new_xyz_file), exist_ok=True)
            write_xyz_file(new_xyz_file, header_xyz, atoms_xyz, mapped_bonds)
            logging.info(f"New XYZ file with bond information saved as {new_xyz_file}")
        else:
            results_df.loc[index, 'Valid_Mapping'] = False
            logging.warning(f"No valid mapping found for {precursor}")
    except KeyError as e:
        logging.error(f"KeyError: {e} - Ensure the 'element' attribute is correctly set in nodes")

# Save the updated results to a new CSV file
results_df.to_csv(valid_csv, index=False)

logging.info(f"Check completed and results saved to '{valid_csv}'")
