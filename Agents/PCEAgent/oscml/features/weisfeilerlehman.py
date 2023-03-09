import collections
import logging
from time import sleep

import numpy as np
from tqdm import tqdm

from oscml.utils.util import smiles2mol, concat

def get_atoms_BFS(graph):
    def bfs(visited, graph, node):
        visited.append(node.GetIdx())
        queue.append(node)
        while queue:
            s = queue.pop(0)
            neighbours = s.GetNeighbors()
            for neighbour in neighbours:
                nidx = neighbour.GetIdx()
                if nidx not in visited:
                    visited.append(nidx)
                    queue.append(neighbour)
        return visited

    visited = []
    queue = []   # Initialize a queue
    start_node = graph.GetAtomWithIdx(0)
    atoms_BFS_order = bfs(visited, graph, start_node)    
    return atoms_BFS_order

def get_atoms_and_bonds(mol, atom_dict, bond_dict):
    atoms = [a.GetSymbol() for a in mol.GetAtoms()]
    for a in mol.GetAromaticAtoms():
        i = a.GetIdx()
        atoms[i] = (atoms[i], 'aromatic')
    atoms = [atom_dict[a] for a in atoms]
    
    i_jbond_dict = collections.defaultdict(lambda: [])
    for b in mol.GetBonds():
        i, j = b.GetBeginAtomIdx(), b.GetEndAtomIdx()
        bond = bond_dict[str(b.GetBondType())]
        i_jbond_dict[i].append((j, bond))
        i_jbond_dict[j].append((i, bond))
        
    return atoms, i_jbond_dict

def extract_fragments(radius, atoms, i_jbond_dict,
                         fingerprint_dict, edge_dict):
    """Extract the fragments from a molecular graph
    based on Weisfeiler-Lehman algorithm.
    """

    if (len(atoms) == 1) or (radius == 0):
        nodes = [fingerprint_dict[a] for a in atoms]

    else:
        nodes = atoms
        i_jedge_dict = i_jbond_dict

        for _ in range(radius):

            """Update each node ID considering its neighboring nodes and edges.
            The updated node IDs are the fingerprint IDs.
            """
            nodes_ = []
            for i, j_edge in i_jedge_dict.items():
                neighbors = [(nodes[j], edge) for j, edge in j_edge]
                fingerprint = (nodes[i], tuple(sorted(neighbors)))
                nodes_.append(fingerprint_dict[fingerprint])

            """Also update each edge ID considering
            its two nodes on both sides.
            """
            i_jedge_dict_ = collections.defaultdict(lambda: [])
            for i, j_edge in i_jedge_dict.items():
                for j, edge in j_edge:
                    both_side = tuple(sorted((nodes[i], nodes[j])))
                    edge = edge_dict[(both_side, edge)]
                    i_jedge_dict_[i].append((j, edge))

            nodes = nodes_
            i_jedge_dict = i_jedge_dict_

    return np.array(nodes)

class Mol2seq_WL():
    
    def __init__(self, radius):
        self.atom_dict = collections.defaultdict(lambda:len(self.atom_dict))
        self.bond_dict = collections.defaultdict(lambda:len(self.bond_dict))
        self.fragment_dict = collections.defaultdict(lambda:len(self.fragment_dict))
        self.edge_dict = collections.defaultdict(lambda: len(self.edge_dict))
        self.radius = radius
        
    def __call__(self, m):
        atoms, i_jbond_dict = get_atoms_and_bonds(m, self.atom_dict, self.bond_dict)
        descriptor = extract_fragments(self.radius, atoms, i_jbond_dict, self.fragment_dict, self.edge_dict)
        atoms_BFS_order = get_atoms_BFS(m)
        descriptor_BFS = [descriptor[i] for i in atoms_BFS_order]
        return descriptor_BFS

def mol2seq(radius, df, column='SMILES_str'):
    logging.info('filling mol2seq according to Weisfeiler Lehman algorithm with radius=' + str(radius))
    sleep(1)
    
    mol2seq = Mol2seq_WL(radius)
    for i in tqdm(range(len(df))):
        smiles = df.iloc[i][column]
        m = smiles2mol(smiles)
        mol2seq(m)
    
    logging.info(concat('atom dict:', len(mol2seq.atom_dict), mol2seq.atom_dict))
    logging.info(concat('bond dict:', len(mol2seq.bond_dict), mol2seq.bond_dict))
    logging.info(concat('fragment dict:', len(mol2seq.fragment_dict), mol2seq.fragment_dict))
    logging.info(concat('edge dict:', len(mol2seq.edge_dict), mol2seq.edge_dict))
    
    return mol2seq