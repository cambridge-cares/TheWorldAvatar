__author__ = "Aleksandar Kondinski"
__license__ = "MIT"  
__version__ = '0.1.0'  
__status__ = "development" 

import logging
import pandas as pd
import os
from autografs.autografs import run
from cof_logic.cof_stacker import COFStacker
from ase.io import read, write
from cof_logic.cof_dftb import cif_to_hd
from cof_logic.cof_dftb import initiate_hsd_file
from cof_logic.cof_dftb import skf_files
from cof_logic.subunit_builder.subunit_operations import SubunitOperations

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
        filtered_cofs_path = os.path.join(self.script_location, 'Data', 'csv', '21072024.csv')
        precursors_inp_path = os.path.join(self.script_location, 'Data', 'csv', 'Precursors_inp.csv')
        linkage_inp_path = os.path.join(self.script_location, 'Data', 'csv', 'Linkages_inp.csv')
        dftb_cofs_path = os.path.join(self.script_location, 'Data', 'DFTB')
        
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
        #gin_path = os.path.join("Data", "Generated_GINs", f"{prefix}.gin")

        # Convert to .cif
        atoms = read(extxyz_file_path)  # Read the .extxyz file
        write(cif_path, atoms)  # Write to .cif
        #write(gin_path, atoms)

        logging.info(f"Saved {prefix} as CIF.")
        return cif_path  # Add this line to return the path of the generated CIF file

    def cof_parameter_extractor(self, output_path, output_ext):
        """
        Extracts parameters from the COFs and precursor data and performs further logic on it.
        
        :param output_path: Path where the output files will be stored.
        :type output_path: str
        :param output_ext: File extension for the output files.
        :type output_ext: str
        """
        if self.filtered_cofs is None or self.precursors_inp is None:
            logging.warning("Data not available, can't extract parameters")
            return

        for _, row in self.filtered_cofs.iterrows():
            # Extract relevant information from the row
            precursor_1, precursor_2, linkage, topology_name, cof_nr, assembly_model = self.extract_info_from_row(row)
            sbu_names, precursor_values, supercell_dimensions = self.get_sbu_names(precursor_1, precursor_2, linkage, assembly_model)            
            self.process_cof(cof_nr, sbu_names, topology_name, output_path, output_ext, precursor_values, supercell_dimensions)

            # Call the separate function for further processing
            # self.process_cof(cof_nr, sbu_names, topology_name, output_path, output_ext)
    
    def extract_info_from_row(self, row):
        """
        Extract relevant information from a row of the DataFrame.
        
        :param row: A row of the DataFrame
        :type row: pd.Series
        :return: A tuple of extracted information
        :rtype: tuple
        """
        assembly_model = row['Assembly_Model']
        precursor_1 = row['Precursor1'] if pd.notna(row['Precursor1']) and row['Precursor1'] != '0' else None
        precursor_2 = row['Precursor2'] if pd.notna(row['Precursor2']) and row['Precursor2'] != '0' else None
        linkage = row['Linkage']
        topology_name = row['Framework']
        cof_nr = row['COF_Nr']
        
        return precursor_1, precursor_2, linkage, topology_name, cof_nr, assembly_model

    def check_and_swap_precursors(self, precursor_1, precursor_2):
        bindingSite_1 = None
        bindingSite_2 = None
        swap_needed = False

        if precursor_1 is not None:
            matched_row = self.precursors_inp[self.precursors_inp['Precursor'] == precursor_1]
            if not matched_row.empty:
                bindingSite_1 = matched_row.iloc[0]['bindingSite']

        if precursor_2 is not None:
            matched_row = self.precursors_inp[self.precursors_inp['Precursor'] == precursor_2]
            if not matched_row.empty:
                bindingSite_2 = matched_row.iloc[0]['bindingSite']

        # Check if swap is needed
        swap_conditions = ["BDOH", "BDNH2", "BDO", 'BDOHNH2', 'OCOCO']
        if bindingSite_2 in swap_conditions and bindingSite_1 not in swap_conditions:
            swap_needed = True

        return swap_needed

   
    def get_sbu_names(self, precursor_1, precursor_2, linkage, assembly_model):
        """
        Get the names of the SBUs and additional information based on the precursors and linkage.
        
        :param precursor_1: The first precursor
        :type precursor_1: str or None
        :param precursor_2: The second precursor
        :type precursor_2: str or None
        :param linkage: The linkage
        :type linkage: str
        :return: A tuple of SBU names and additional information
        :rtype: tuple
        """
        print(linkage)
        print(assembly_model)
        sbu_names = []
        supplementary_sbu = None
        precursor_values = {}
        supercell_dimensions = None
        swap_needed = self.check_and_swap_precursors(precursor_1, precursor_2)
        
        if swap_needed:
        # Swap precursor_1 and precursor_2
            precursor_1, precursor_2 = precursor_2, precursor_1
        
        list_0_assembly_models = ['hcb-[(2-linear)x3(L:3-planar)x2]n',
                                'bcu-[(2-linear)x4(L:8-cube)x1]n',
                                'dia-[(4-tetrahedral)x1(L:2-linear)x2]n',
                                'sql-[(4-planar)x1(L:2-linear)x2]n',
                                'kgm-[(4-planar)x1(L:2-linear)x2]n']
        
        list_1_assembly_models = ['ctn-[(4-tetrahedral)x3(L:3-planar)x4]n',
                                'hcb-[(3-planar)x1(L:3-planar)x1]n',
                                'hnb-[(3-planar)x1(L:3-planar)x1]n'] 
       
        list_2_assembly_models = ['dia-[(4-tetrahedral)x1(2-linear)x2(L:2-linear)x4]n',
                                'hxl-[(6-planar)x1(2-linear)x3(L:2-linear)x6]n',
                                'hcb-[(3-planar)x2(2-linear)x3(L:2-linear)x6]n',
                                'hcb-[(3-pyramidal)x2(2-linear)x3(L:2-linear)x6]n',
                                'kgm-[(4-planar)x1(2-linear)x2(L:2-linear)x4]n',
                                'sql-[(4-planar)x1(2-linear)x2(L:2-linear)x4]n'] 
        
        list_3_assembly_models = ['acs-[(6-prism)x1(6-prism)x1(L:2-linear)x6]n',
                                'stp-[(6-planar)x2(4-planar)x3(L:2-linear)x12]n',
                                'ctn-[(4-tetrahedral)x3(3-planar)x4(L:2-linear)x12]n',
                                'tbo-[(4-planar)x3(3-planar)x4(L:2-linear)x12]n',
                                'bor-[(4-tetrahedral)x3(3-planar)x4(L:2-linear)x12]n',
                                'ffc-[(4-planar)x3(3-planar)x4(L:2-linear)x12]n',
                                'dia-[(4-tetrahedral)x1(4-tetrahedral)x1(L:2-linear)x4]n',
                                'kgd-[(6-planar)x1(3-planar)x2(L:2-linear)x6]n',
                                'pts-[(4-tetrahedral)x1(4-planar)x1(L:2-linear)x4]n', 
                                'sql-[(4-planar)x1(4-planar)x1(L:2-linear)x4]n',
                                'hcb-[(3-planar)x1(3-planar)x1(L:2-linear)x3]n',
                                'hca-[(3-planar)x1(3-planar)x1(L:2-linear)x3]n']                   
        
        #'cpt-[(6-planar)x1(2-linear)x3(L:2-linear)x6]n',
        #'ceq-[(6-prism)x1(3-planar)x2(L:2-linear)x6]n',
  
        lfr_set_0 = ['LFR-16', 'LFR-17', 'LFR-19'] # branching linkages, reactions with one precursor
        lfr_set_1 = ['LFR-20','LFR-21'] #symmetrical linear linages 
        lfr_set_2 = ['LFR-6','LFR-12','LFR-13','LFR-18'] # single bond
        lfr_set_3 = ['LFR-10', 'LFR-50','LFR-40'] #symmetrical building unit, carbon carbon
        lfr_set_4 = ['LFR-3', 'LFR-8', 'LFR-1', 'LFR-7', 'LFR-4', 'LFR-5','LFR-35',] # ,# symmetric-assymetric two atom bridges involved 
        lfr_set_5 = ['LFR-2', 'LFR-11', 'LFR-24']  # , 'LFR-30', 'LFR-31'  assumetric linear linkages
        
        if assembly_model == 'sql-[(4-planar)x1(4-planar)x1(L:2-linear)x4]n':
            supercell_dimensions =  supercell_dimensions = (2, 2, 1)
        else:
            supercell_dimensions = (1, 1, 1)
        
        supplementary_sbu = 'dum_dum'
        if assembly_model in list_0_assembly_models:
            sbu_names = [linkage]
            if linkage == 'LFR-21':
                sbu_names.append('LFR-21_rev')
            else:
                pass
        else:
            pass
        
        if assembly_model in list_1_assembly_models:
            sbu_names = [supplementary_sbu]
            sbu_names.append(linkage) 
        else:
            pass
            
        if precursor_1 is not None:
            matched_row = self.precursors_inp[self.precursors_inp['Precursor'] == precursor_1]
            for _, m_row in matched_row.iterrows():
                sbu_1 = m_row['inp']
                sbu_names.append(sbu_1)
                values = {key: m_row[key] for key in ['GBU', 'bindingSite', 'bsIndex', 'Dentation']}
                precursor_values['Precursor_1'] = values
                            
        if precursor_2 is not None:
            matched_row = self.precursors_inp[self.precursors_inp['Precursor'] == precursor_2]
            for _, m_row in matched_row.iterrows():
                sbu_names.append(m_row['inp'])
                values = {key: m_row[key] for key in ['GBU', 'bindingSite', 'bsIndex', 'Dentation']}
                precursor_values['Precursor_2'] = values

        if assembly_model in list_2_assembly_models:
            if linkage in lfr_set_2:
                pass             
            else:
                subunit_operations = SubunitOperations(sbu_names[0], linkage, precursor_values['Precursor_1']['bindingSite'])
                subunit_result = subunit_operations.process()
                sbu_names[0] = subunit_result
                   
        if assembly_model in list_3_assembly_models:
            if linkage in lfr_set_2:
                sbu_names.append(supplementary_sbu)
            else:
                subunit_operations = SubunitOperations(sbu_names[0], linkage, precursor_values['Precursor_1']['bindingSite'])
                subunit_result = subunit_operations.process()
                sbu_names[0] = subunit_result
                sbu_names.append(supplementary_sbu)

        print('---------LOOK HERE----------')
        print(sbu_names)
        print(precursor_values)
        print('---------LOOK HERE----------')
        
        return sbu_names, precursor_values, supercell_dimensions

    def process_cof_X(self, cof_nr, sbu_names, topology_name, output_path, output_ext, precursor_values, supercell_dimensions):
       
        #print(precursor_values)
        output_file_path = os.path.join(output_path, f'generated_cof_{cof_nr}')
        logging.info(f'RUNNING AUTOGRAFS FOR COF number: {cof_nr}')
        run(sbu_names, topology_name, output_file_path, output_ext, supercell_dimensions)
        
        logging.info(f'Geometry of COF {cof_nr} successfully Generated')

        extxyz_file_path = f"{output_file_path}.inp"
        inp_file_path = f"{output_file_path}.inp"


    def process_cof(self, cof_nr, sbu_names, topology_name, output_path, output_ext, precursor_values, supercell_dimensions):
        """
        Perform further logic on the COF.
        
        :param cof_nr: COF number
        :type cof_nr: int
        :param sbu_names: List of SBU names
        :type sbu_names: list
        :param topology_name: Topology name
        :type topology_name: str
        :param output_path: Path where the output files will be stored
        :type output_path: str
        :param output_ext: File extension for the output files
        :type output_ext: str
        """
        
        #print(precursor_values)
        output_file_path = os.path.join(output_path, f'generated_cof_{cof_nr}')
        logging.info(f'RUNNING AUTOGRAFS FOR COF number: {cof_nr}')
        run(sbu_names, topology_name, output_file_path, output_ext, supercell_dimensions)
        

        logging.info(f'Geometry of COF {cof_nr} successfully Generated')

        extxyz_file_path = f"{output_file_path}.extxyz"
        inp_file_path = f"{output_file_path}.inp"
        #if self.is_2D_COF(extxyz_file_path):
        #    logging.info(f'STACKING OF COF {cof_nr} in AA and AB')
            #cof_stacker = COFStacker(None) 
            #cof_stacker.stack_cof(cof_nr, input_filename=f"generated_cof_{cof_nr}")
            #cof_stacker = COFStacker(f"generated_cof_{cof_nr}")
            #cof_stacker.stack_cof(cof_nr)
            
        #else:
        #    cif_file = self.write_and_convert_to_cif(f"COF_{cof_nr}", extxyz_file_path)
            
            # Create a new directory named after the COF in the DFTB directory
        #    dftb_cof_path = os.path.join(self.script_location, 'Data', 'DFTB', f"{cof_nr}")
        #    os.makedirs(dftb_cof_path, exist_ok=True)

            # Convert CIF to GEN format and save it in the new directory
        #    gen_file = os.path.join(dftb_cof_path, 'geo_end.gen')
         #   cif_to_hd(cif_file, gen_file)

            # Initiate the HSD file
       #     hsd_file_path = os.path.join(dftb_cof_path, 'dftb_in.hsd')
       #     initiate_hsd_file(gen_file, skf_files, output_filename=hsd_file_path)