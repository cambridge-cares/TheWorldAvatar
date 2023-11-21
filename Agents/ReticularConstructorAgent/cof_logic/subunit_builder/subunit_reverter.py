__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import time
import os
import json

class SubunitReverter:
    def __init__(self, json_file_path, output_file_path, shape, pg, name):
        self.json_file_path = json_file_path
        self.output_file_path = output_file_path
        self.shape = shape
        self.pg = pg
        self.name = name

    def create_row_mapping(self, data):
        """ Create a mapping from atom UUID to row number. """
        return {uuid: row_number for row_number, uuid in enumerate(data, 1)}

    def process_bonds(self, bonds, row_mapping):
        """ Process bond information in 'sandwich' style. """
        if bonds:
            first_bond_atom = str(row_mapping[bonds[0]['to_atom']])
            first_bond_order = str(bonds[0]['bond_order'])
            if len(bonds) > 1:
                middle_bonds = '/'.join(f"{bond['bond_order']}:{row_mapping[bond['to_atom']]}" for bond in bonds[1:])
                bond_str = f"BOND={first_bond_atom}/{middle_bonds}/{first_bond_order}"
            else:
                bond_str = f"BOND={first_bond_atom}/{first_bond_order}"
        else:
            bond_str = ''
        return bond_str

    def process_json_to_custom_format_with_bonds(self, json_file_path):
        """ Process the JSON file to a custom format, including 'sandwich' bond information with formatted coordinates. """
        with open(self.json_file_path, 'r') as file:
            data = json.load(file)

        row_mapping = self.create_row_mapping(data)

        with open(self.output_file_path, 'w') as output_file:
            # Write additional data at the top
            output_file.write(f"Data: shape = {self.shape}\n")
            output_file.write(f"Data: pg = {self.pg}\n")
            output_file.write(f"Data: name  = {self.name}\n")
            output_file.write("GEOMETRY CARTESIAN\n")

            for row_number, (uuid, atom_data) in enumerate(data.items(), 1):
                atom = atom_data['atom']
                # Format coordinates with six decimal places
                coords = [f"{atom_data['coordinate_x']:.6f}", f"{atom_data['coordinate_y']:.6f}", f"{atom_data['coordinate_z']:.6f}"]
                mmtype = atom_data.get('mmtype', '')
                qmmm = atom_data.get('qmmm', '')

                # Process bond information in 'sandwich' style
                bond_str = self.process_bonds(atom_data.get('bond', []), row_mapping)

                # Write the formatted data to the output file
                coords_str = ' '.join(f"{c:<12}" for c in coords)
                output_file.write(f"{atom:<6} {coords_str}MMTYPE={mmtype} QMMM={qmmm} {bond_str}\n")

            # Write "END" at the bottom
            output_file.write("END\n")

if __name__ == "__main__":
    # User input for paths and additional data
    json_file_path = 'path/to/subcomponent.json'
    output_file_path = 'path/to/formatted_output_with_sandwich_bonds.inp'
    shape = '[1 1 0 1 1 2]'
    pg = 'D*h'
    name = 'LFR-3'

    reverter = SubunitReverter(json_file_path, output_file_path, shape, pg, name)
    reverter.process_json_to_custom_format_with_bonds()
