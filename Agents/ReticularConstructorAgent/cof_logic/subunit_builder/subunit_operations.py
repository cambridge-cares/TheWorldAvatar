__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import os
import datetime
import logging
from subunit_converter import SubunitConverter
from subunit_builder import SubunitBuilder
from subunit_reverter import SubunitReverter

class SubunitOperations:
    
    def __init__(self, core_inp_name, lfr_inp_name, bs_type):
        self.core_inp_name = core_inp_name
        self.lfr_inp_name = lfr_inp_name
        self.bs_type = bs_type

        # Set up directories
        self.script_directory = os.path.dirname(os.path.abspath(__file__))
        self.inp_files_directory = os.path.normpath(os.path.join(self.script_directory, r'..\..\autografs\database\database'))
        self.base_output_directory = os.path.join(self.script_directory, r'..\..\Data\Subunits')

        # Set up logging
        self.setup_logging()
        
    @staticmethod
    def setup_logging():
        logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
        # For more advanced logging (e.g., to a file), you can customize this method.

    @staticmethod
    def read_first_two_lines(inp_file_path):
        with open(inp_file_path, 'r') as file:
            lines = [next(file) for _ in range(2)]
        return lines

    def process(self):
        logging.info("Starting processing of input files.")
        core_inp_file_path = os.path.join(self.inp_files_directory, self.core_inp_name)
        shape_line, pg_line = self.read_first_two_lines(core_inp_file_path)
        shape = shape_line.split('=')[1].strip()
        pg = pg_line.split('=')[1].strip()

        current_time = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        temp_output_directory = os.path.join(self.base_output_directory, f"temp_output_{current_time}")
        os.makedirs(temp_output_directory, exist_ok=True)
        
        logging.info("Converting and building subunits.")

        converter = SubunitConverter(self.inp_files_directory, temp_output_directory, self.core_inp_name, self.lfr_inp_name)
        converter.process_files(None, self.bs_type)

        builder = SubunitBuilder()
        core_json_path = os.path.join(temp_output_directory, 'core.json')
        lfr_folder_path = temp_output_directory
        core_atoms, reference_atoms = builder.shift_atoms_to_origin(core_json_path)
        builder.update_lfr_files(core_atoms, reference_atoms, lfr_folder_path, self.bs_type)
        subcomponent_json_path = os.path.join(lfr_folder_path, 'subcomponent.json')
        subcomponent_xyz_path = os.path.join(lfr_folder_path, 'subcomponent.xyz')
        builder.create_and_update_subcomponent(core_json_path, subcomponent_json_path, lfr_folder_path)
        builder.generate_xyz_from_json(subcomponent_json_path, subcomponent_xyz_path)

        output_file_path = os.path.join(temp_output_directory, f"{current_time}.inp")
        reverter = SubunitReverter(subcomponent_json_path, output_file_path, shape, pg, current_time)
        reverter.process_json_to_custom_format_with_bonds()

        logging.info(f"New inp file saved at: {output_file_path}")

if __name__ == "__main__":
    processor = SubunitOperations("4P-XX-40.inp", "LFR-2.inp", "NH2")
    processor.process()