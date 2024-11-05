__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import json
import os

class JSONToXYZConverter:
    def __init__(self, input_directory, output_file):
        self.input_directory = input_directory
        self.output_file = output_file

    def collect_atoms(self):
        atoms = []
        for file_name in os.listdir(self.input_directory):
            if file_name.endswith('.json'):
                file_path = os.path.join(self.input_directory, file_name)
                #print(f"Reading file: {file_path}") 
                with open(file_path, 'r') as file:
                    data = json.load(file)
                    for atom_data in data.values():
                        if atom_data['atom'] not in ['CENTER', 'X']:
                            atoms.append(atom_data)
        #print(f"Collected {len(atoms)} atoms") 
        return atoms

    def write_xyz(self, atoms):
        with open(self.output_file, 'w') as file:
            file.write(f"{len(atoms)}\n\n")  # XYZ format header
            for atom in atoms:
                file.write(f"{atom['atom']} {atom['coordinate_x']} {atom['coordinate_y']} {atom['coordinate_z']}\n")

    def convert(self):
        #print(f"Starting conversion from JSON to XYZ in directory: {self.input_directory}")  # Debugging statement
        atoms = self.collect_atoms()
        self.write_xyz(atoms)
        #print(f"XYZ file has been written to {self.output_file}")

if __name__ == "__main__":
    input_directory = 'Data/Assembly_Models/Translated_CBUs/'  # Directory containing the JSON files
    output_file = 'output_structure.xyz'  # Output XYZ file

    converter = JSONToXYZConverter(input_directory, output_file)
    converter.convert()
