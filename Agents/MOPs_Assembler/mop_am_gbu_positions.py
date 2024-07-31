__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import json
import os

class PositionExtractor:

    """
    PositionExtractor esentially extracts the positions of the virtual rescaled generic building units.
    """

    def __init__(self, input_file, assembly_model_key, output_dir):
        self.input_file = input_file
        self.assembly_model_key = assembly_model_key
        self.output_dir = output_dir

    def create_output_dir(self):
        os.makedirs(self.output_dir, exist_ok=True)

    def load_json_data(self):
        with open(self.input_file, 'r') as file:
            return json.load(file)

    def extract_positions(self, data):
        unique_positions = {}
        dummy_positions = {atom['Key']: (atom['X'], atom['Y'], atom['Z']) for atom in data[self.assembly_model_key] if atom['Label'] == 'Dummy'}
        
        for atom in data[self.assembly_model_key]:
            if atom['Label'] not in ['Dummy', 'Center']:
                closest_dummies = {dummy: dummy_positions[dummy] for dummy in atom.get('ClosestDummies', []) if dummy in dummy_positions}
                unique_positions[atom['Key']] = {
                    'Key': atom['Key'],
                    'Label': atom['Label'],
                    'X': atom['X'],
                    'Y': atom['Y'],
                    'Z': atom['Z'],
                    'ClosestDummies': closest_dummies
                }
        return unique_positions

    def write_positions_to_files(self, unique_positions):
        for key, pos in unique_positions.items():
            output_file_path = os.path.join(self.output_dir, f"{key}.json")
            with open(output_file_path, 'w') as output_file:
                json.dump(pos, output_file, indent=4)

    def run(self):
        self.create_output_dir()
        data = self.load_json_data()
        unique_positions = self.extract_positions(data)
        self.write_positions_to_files(unique_positions)
        print(f"Exported {len(unique_positions)} JSON files to {self.output_dir}")

if __name__ == "__main__":
    input_file = 'Data/Assembly_Models/(4-planar)x6(2-bent)x12_Oh_final_scaled.json'
    output_dir = 'Data/Assembly_Models/Output_Atoms'
    assembly_model_key = '(4-planar)x6(2-bent)x12_Oh'

    extractor = PositionExtractor(input_file, assembly_model_key, output_dir)
    extractor.run()
