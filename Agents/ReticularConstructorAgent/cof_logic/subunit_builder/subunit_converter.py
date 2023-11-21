__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import uuid
import json
import os
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class SubunitConverter:
    
    def __init__(self, directory, output_directory, core_inp_name, lfr_inp_name):
        self.directory = directory
        self.output_directory = output_directory
        self.core_inp_name = core_inp_name
        self.lfr_inp_name = lfr_inp_name
        self.core_file, self.lfr_file = self.find_inp_files()
        
    def parse_line(self, line, uuid_mapping):
        parts = line.split()
        data = {
            "atom": parts[0],
            "coordinate_x": float(parts[1]),
            "coordinate_y": float(parts[2]),
            "coordinate_z": float(parts[3]),
            "bond": []
        }
        for part in parts[4:]:
            key, value = part.split('=')
            if key.lower() == 'bond':
                for bond_info in value.split(':'):
                    to_atom, bond_order = bond_info.split('/')
                    to_atom_uuid = uuid_mapping.get(int(to_atom))
                    if to_atom_uuid:
                        data['bond'].append({
                            "to_atom": to_atom_uuid,
                            "bond_order": float(bond_order)
                        })
            else:
                data[key.lower()] = value
        return data

    def generate_uuids_and_parse_inp(self, file_path):
        with open(file_path, 'r') as f:
            lines = f.readlines()

        geometry_data = {}
        uuid_mapping = {}
        recording = False
        index = 1

        for line in lines:
            line = line.strip()
            if line == "GEOMETRY CARTESIAN":
                recording = True
            elif line == "END":
                recording = False
            elif recording and line:
                unique_id = str(uuid.uuid4())
                uuid_mapping[index] = unique_id
                index += 1

        index = 1
        for line in lines:
            line = line.strip()
            if line == "GEOMETRY CARTESIAN":
                recording = True
            elif line == "END":
                recording = False
            elif recording and line:
                unique_id = uuid_mapping[index]
                parsed_data = self.parse_line(line, uuid_mapping)
                geometry_data[unique_id] = parsed_data
                index += 1

        return geometry_data
    
    def find_inp_files(self):
        core_file = os.path.join(self.directory, self.core_inp_name)
        lfr_file = os.path.join(self.directory, self.lfr_inp_name)
        
        # Check if files exist
        if not os.path.exists(core_file) or not os.path.exists(lfr_file):
            raise FileNotFoundError(f"Input files not found: {core_file}, {lfr_file}")

        return core_file, lfr_file
    
    def process_files(self, bs_denticity, bs_type):
        if self.core_file is None or self.lfr_file is None:
            logging.error("Missing required inp files.")
            return
        
        geometry_data_core = self.generate_uuids_and_parse_inp(self.core_file)
        num_x_atoms_core = sum(1 for data in geometry_data_core.values() if data['atom'] == 'X')
        modularity_core = num_x_atoms_core
        
        with open(os.path.join(self.output_directory, "core.json"), 'w') as f:
            json.dump(geometry_data_core, f, indent=4)
        logging.info("Data for the CORE input file has been written to core.json")
        
        for i in range(modularity_core):
            geometry_data_lfr = self.generate_uuids_and_parse_inp(self.lfr_file)
            output_path = os.path.join(self.output_directory, f"lfr_copy_{i+1}.json")
            with open(output_path, 'w') as f:
                json.dump(geometry_data_lfr, f, indent=4)
            logging.info(f"Data for copy {i+1} has been written to {output_path}")

def main():
    directory = r'C:\TheWorldAvatar\Agents\ReticularConstructorAgent\cof_logic\subunit_builder'
    output_directory = r'C:\TheWorldAvatar\Agents\ReticularConstructorAgent\cof_logic\subunit_builder\output'
    bs_denticity = "Monodentate"
    bs_type = "NH2"
    
    converter = SubunitConverter(directory, output_directory)
    converter.process_files(bs_denticity, bs_type)

if __name__ == "__main__":
    main()
