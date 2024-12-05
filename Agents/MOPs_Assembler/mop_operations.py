__author__ = "Aleksandar Kondinski"
__license__ = "MIT"
__version__ = '1.0.0'
__status__ = "production"

import csv
import os
import json
from datetime import datetime
from mop_am_rescaler import AssemblyModelRescaler
from mop_am_gbu_positions import PositionExtractor
from mop_cbu_aligner import GenericCBUProcessor
from mop_jsons2xyz import JSONToXYZConverter

class AssemblerWorkflow:
    def __init__(self, assembly_model_file, cbus_dir, output_dir):
        self.assembly_model_file = assembly_model_file
        self.cbus_dir = cbus_dir
        self.output_dir = output_dir

    def run_workflow(self, file_path):
        matched_files = []

        if not os.path.exists(file_path):
            print(f"File not found: {file_path}")
            return

        try:
            # Generate a timestamp for the current run
            run_timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')

            with open(file_path, mode='r') as file:
                csv_reader = csv.reader(file)
                headers = next(csv_reader)  # Skip the header row
                for row in csv_reader:
                    print(', '.join(row))

                    mop_id = row[0].strip()
                    assembly_model = row[1].strip()
                    cbu1 = row[2].strip()
                    cbu1_label = row[3].strip()
                    cbu2 = row[4].strip()
                    cbu2_label = row[5].strip()

                    assembly_model_path = self.assembly_model_file
                    cbu1_path = os.path.join(self.cbus_dir, f"{cbu1}.json")
                    cbu2_path = os.path.join(self.cbus_dir, f"{cbu2}.json")

                    # Include timestamp in the output directory name
                    mop_output_dir = os.path.join(self.output_dir, f"{mop_id}_{run_timestamp}")
                    os.makedirs(mop_output_dir, exist_ok=True)

                    matched_files.append((assembly_model_path, assembly_model, cbu1_label, cbu2_label, cbu1_path, cbu2_path, mop_output_dir))

        except Exception as e:
            print(f"An error occurred: {e}")

        # Process each MOP entry
        if matched_files:
            for files in matched_files:
                assembly_model_path, assembly_model, cbu1_label, cbu2_label, cbu1_path, cbu2_path, mop_output_dir = files
                self.process_mop_entry(assembly_model_path, assembly_model, cbu1_label, cbu2_label, cbu1_path, cbu2_path, mop_output_dir)
        else:
            print("No matching files found for workflow operations.")

    def process_mop_entry(self, assembly_model_path, assembly_model, cbu1_label, cbu2_label, cbu1_path, cbu2_path, mop_output_dir):
        # Load the full assembly model JSON
        with open(assembly_model_path, 'r') as file:
            assembly_models = json.load(file)
        
        # Extract the specific section for the current assembly model
        assembly_model_data = assembly_models[assembly_model]

        # Save the extracted section to a temporary file
        temp_assembly_model_path = os.path.join(mop_output_dir, f"{assembly_model}_temp.json")
        with open(temp_assembly_model_path, 'w') as file:
            json.dump({assembly_model: assembly_model_data}, file, indent=4)

        # Rescale the extracted section
        rescaled_dir = os.path.join(mop_output_dir, 'Rescaled')
        os.makedirs(rescaled_dir, exist_ok=True)

        rescaler = AssemblyModelRescaler(temp_assembly_model_path, assembly_model, cbu1_label, cbu2_label, cbu1_path, cbu2_path)
        rescaler.workflow_operations(rescaled_dir)

        final_scaled_json_path = os.path.join(rescaled_dir, f"{os.path.basename(temp_assembly_model_path).replace('.json', '_final_scaled.json')}")
        gbu_positions_dir = os.path.join(mop_output_dir, 'GBU_Positions')
        os.makedirs(gbu_positions_dir, exist_ok=True)

        extractor = PositionExtractor(final_scaled_json_path, assembly_model, gbu_positions_dir)
        extractor.run()

        # Process positions based on labels
        position_files = [os.path.join(gbu_positions_dir, file) for file in os.listdir(gbu_positions_dir) if file.startswith('Position_') and file.endswith('.json')]
        print(f"Collected position files: {position_files}")

        translated_cbu_dir = os.path.join(mop_output_dir, 'Translated_CBUs')
        os.makedirs(translated_cbu_dir, exist_ok=True)

        for position_file in position_files:
            with open(position_file, 'r') as f:
                position_data = json.load(f)
                label = position_data['Label']
                if label == cbu1_label:
                    cbu_processor = GenericCBUProcessor(cbu1_path, [position_file], cbu1_label)
                    cbu_processor.process()
                elif label == cbu2_label:
                    cbu_processor = GenericCBUProcessor(cbu2_path, [position_file], cbu2_label)
                    cbu_processor.process()
                else:
                    print(f"Unknown label: {label} for position {position_file}")

        converter = JSONToXYZConverter(translated_cbu_dir, os.path.join(mop_output_dir, 'output_structure.xyz'))
        converter.convert()
