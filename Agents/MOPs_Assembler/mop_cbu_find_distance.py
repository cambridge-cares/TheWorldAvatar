__author__ = "Aleksandar Kondinski"
__license__ = "MIT"
__version__ = '1.0.0'
__status__ = "production"

import json
import math
from itertools import combinations

class MoleculeAnalyzer:

    def __init__(self, json_file):
        self.json_file = json_file
        self.data = self.load_json()
        self.dummy_atoms = self.find_dummy_atoms()
    
    def load_json(self):
        with open(self.json_file, 'r') as f:
            return json.load(f)
    
    def find_dummy_atoms(self):
        dummy_atoms = {}
        for uuid, atom_data in self.data.items():
            if 'atom' in atom_data and atom_data['atom'] == 'X':
                dummy_atoms[uuid] = atom_data
        return dummy_atoms
    
    def calculate_distance(self, atom1, atom2):
        dx = atom1['coordinate_x'] - atom2['coordinate_x']
        dy = atom1['coordinate_y'] - atom2['coordinate_y']
        dz = atom1['coordinate_z'] - atom2['coordinate_z']
        return math.sqrt(dx*dx + dy*dy + dz*dz)
    
    def analyze_distances(self):
        distances = []
        dummy_atom_ids = list(self.dummy_atoms.keys())
        
        for atom1_id, atom2_id in combinations(dummy_atom_ids, 2):
            atom1 = self.dummy_atoms[atom1_id]
            atom2 = self.dummy_atoms[atom2_id]
            distance = self.calculate_distance(atom1, atom2)
            distances.append(distance)
        
        return distances
    
    def categorize_distances(self, distances):
        distances.sort()
        num_distances = len(distances)
        
        if num_distances == 1:
            return distances, []
        elif num_distances == 3:
            return distances[:1], distances[1:]
        elif num_distances >= 6:
            threshold = (distances[-1] + distances[-2] + distances[-3]) / 3
            sides = [d for d in distances if d <= threshold]
            diagonals = [d for d in distances if d > threshold]
            return sides, diagonals
        else:
            threshold = (distances[-1] + distances[-2]) / 2
            sides = [d for d in distances if d <= threshold]
            diagonals = [d for d in distances if d > threshold]
            return sides, diagonals
    
    def calculate_average(self, distances):
        if distances:
            return sum(distances) / len(distances)
        return 0.0
    
    def process(self):
        num_dummy_atoms = len(self.dummy_atoms)
        if num_dummy_atoms < 2:
            print("Not enough dummy atoms to calculate distances.")
            return None, None
        
        distances = self.analyze_distances()
        
        sides, diagonals = self.categorize_distances(distances)
        
        avg_side_distance = self.calculate_average(sides)
        avg_diagonal_distance = self.calculate_average(diagonals)
        
        return avg_side_distance, avg_diagonal_distance
