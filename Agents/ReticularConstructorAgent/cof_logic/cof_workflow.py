__author__ = "Aleksandar Kondinski"
__license__ = "MIT"  
__version__ = '0.1.0'  
__status__ = "development" 

import logging
import pandas as pd
import os
from autografs.autografs import run
from .cof_stacker import COFStacker
from ase.io import read, write

class COFProcessor:
    def __init__(self, script_location=None):
        """
        Initializes the COFProcessor class.

        :param script_location: The location of the script. If None, it will be set to the current script location.
        :type script_location: str, optional
        """
        # Set the script location to the provided path or the path of the current script file
        self.script_location = script_location if script_location else os.path.dirname(os.path.abspath(__file__))
        self.filtered_cofs = None  # Placeholder for data about COFs
        self.precursors_inp = None  # Placeholder for data about precursors
    
    def cof_data_reader(self):
        """
        Reads in the filtered COFs and precursor data from CSV files and stores them for later use.
        """
        # Define paths to the CSV files containing COF and precursor data
        filtered_cofs_path = os.path.join(self.script_location, 'Data', 'csv', 'Filtered_COFs.csv')
        precursors_inp_path = os.path.join(self.script_location, 'Data', 'csv', 'Precursors_inp.csv')

        try:
            # Try to read the CSV files and store them as pandas DataFrames
            self.filtered_cofs = pd.read_csv(filtered_cofs_path)
            self.precursors_inp = pd.read_csv(precursors_inp_path)
        except FileNotFoundError as e:
            # Handle the error when CSV files are not found and print a message
            logging.error(f'File not found: {str(e)}')
            # Set the data attributes to None in case of error
            self.filtered_cofs = self.precursors_inp = None
    
    def is_2D_COF(self, file_path):
        """
        Determines whether a COF is 2D based on its .extxyz file.

        :param file_path: Path to the .extxyz file.
        :type file_path: str
        :return: True if the COF is 2D, False otherwise.
        :rtype: bool
        """
        try:
            with open(file_path, 'r') as f:
                lattice_line = f.readlines()[1]  # Assuming the lattice and pbc info is always on the second line

                # Extract the third lattice vector and pbc values
                lattice_info = lattice_line.split('Lattice="')[1].split('"')[0].split()
                lattice_z = list(map(float, lattice_info[6:9]))
                pbc = lattice_line.split('pbc="')[1].split('"')[0].split()

                # Check if it is 2D by your criteria
                return all(val == 0 for val in lattice_z) and pbc == ['T', 'T', 'F']
        except Exception as e:
            logging.error(f'File not found: {str(e)}')
            return False

    def write_and_convert_to_cif(self, prefix, extxyz_file_path):
        """
        Convert .extxyz file to .cif format.
        
        :param prefix: Prefix for naming the CIF file.
        :type prefix: str
        :param extxyz_file_path: Path of the .extxyz file.
        :type extxyz_file_path: str
        """
        # Define relative paths
        cif_path = os.path.join("Data", "Generated_CIFs", f"{prefix}.cif")
        
        # Convert to .cif
        atoms = read(extxyz_file_path)  # Read the .extxyz file
        write(cif_path, atoms)  # Write to .cif

        logging.info(f"Saved {prefix} as CIF.")

    def cof_parameter_extractor(self, output_path, output_ext):
        """
        Extracts parameters from the COFs and precursor data and performs further logic on it.
        
        :param output_path: Path where the output files will be stored.
        :type output_path: str
        :param output_ext: File extension for the output files.
        :type output_ext: str
        """
        # Check if the necessary data is available
        if self.filtered_cofs is None or self.precursors_inp is None:
            logging.warning("Data not available, can't extract parameters")
            return
        
        # Iterate through the rows of the filtered COFs data
        for _, row in self.filtered_cofs.iterrows():
            # Initialize precursors and linkage
            precursor_1 = None
            precursor_2 = None
            linkage = row['Linkage']

            # Check if Precursor1 and Precursor2 are non-NaN and not '0', and assign them if true
            if pd.notna(row['Precursor1']) and row['Precursor1'] != '0':
                precursor_1 = row['Precursor1']
            if pd.notna(row['Precursor2']) and row['Precursor2'] != '0':
                precursor_2 = row['Precursor2']

            # Create a list for storing sbu names, initially containing the linkage
            sbu_names = [linkage]
            # Check if precursor_1 and precursor_2 exist, and if so, append their corresponding 'inp' values from precursors_inp to sbu_names
            if precursor_1 is not None:
                matched_row = self.precursors_inp[self.precursors_inp['Precursor'] == precursor_1]
                for _, m_row in matched_row.iterrows():
                    sbu_names.append(m_row['inp'])
            if precursor_2 is not None:
                matched_row = self.precursors_inp[self.precursors_inp['Precursor'] == precursor_2]
                for _, m_row in matched_row.iterrows():
                    sbu_names.append(m_row['inp'])
            
            # Extract topology name and COF number from the current row of filtered COFs data
            topology_name = row['Framework']
            cof_nr = row['COF_Nr']
            
            # Construct the output file path and call the run function to perform further logic
            output_file_path = os.path.join(output_path, f'generated_cof_{cof_nr}')
            logging.info(f'RUNNING AUTOGRAFS FOR COF number: {cof_nr}')
            run(sbu_names, topology_name, output_file_path, output_ext)
            # Checking if the COF is 2D based on the .extxyz file
            logging.info(f'Geometry of COF {cof_nr} succesfuly Generated')
            extxyz_file_path = f"{output_file_path}.extxyz"  # Assuming the output from run is .extxyz
            if self.is_2D_COF(extxyz_file_path):
                # Stacking for 2D COFs
                logging.info(f'STACKING OF COF {cof_nr} in AA and AB')
                cof_stacker = COFStacker(f"generated_cof_{cof_nr}")
                cof_stacker.stack_cof(cof_nr)
            else:
                self.write_and_convert_to_cif(f"COF_{cof_nr}", extxyz_file_path)