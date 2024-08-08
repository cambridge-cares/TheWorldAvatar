import os
import sys
import shutil
from urllib.parse import urlparse
import uuid
import csv
import re
import time
from update_kg import UpdateKG
from update_kg import config_a_box_updates

class MOPGeometryUpdater(UpdateKG):

    def query_ccdc_numbers(self):
        where_ccdc          = "?MOPIRI om:hasCCDCNumber ?CCDCNum"
        select_ccdc         = "?MOPIRI ?CCDCNum"
        return self.query_triple(where_ccdc, select_ccdc)
    def query_cbu_iri(self):
        where_ccdc          = "?MOPIRI om:hasCBUFormula ?CCDCNum"
        select_ccdc         = "?MOPIRI ?CCDCNum"
        return self.query_triple(where_ccdc, select_ccdc)
    
    def query_mop_iri(self, mop_formula, symmetry):
        print(mop_formula, symmetry)
        where_ccdc          = f"""?MOPIRI om:hasMOPFormula            "{mop_formula}"      ;
                                        om:hasAssemblyModel         ?AM                 .
                                ?AM     om:hasSymmetryPointGroup    "{symmetry}"          .
                                """
        select_ccdc         = "?MOPIRI"
        return self.query_triple(where_ccdc, select_ccdc)
    
    def read_mops_data(self, file_path):
        failed_geometry_entries                 = []
        
        with open(file_path, mode='r', newline='') as csvfile:
            reader = csv.DictReader(csvfile)
            
            for row in reader:
                # check if the current row is a mop with additional (statement 1) and new (statement 2) geometry.
                if row['Mops_Geometry'] != "no_geometry" and contains_underscore(row['Mops_Geometry']) and row['MOP Formula'] != "[Co3]8[(CHC6HO3)4(C3H6OH)4]6":
                    print("mop geom: ", row['Mops_Geometry'])
                    mop_iri                     = self.query_mop_iri(row['MOP Formula'], row["Symmetry Point Group"])
                    if mop_iri == []:
                        reversed_mop            = reverse_mopformula(row['MOP Formula'])
                        print("original mop: ", row['MOP Formula'],"reversed mop: ", reversed_mop)
                        mop_iri                 = self.query_mop_iri(reversed_mop, row["Symmetry Point Group"])
                        print("mop iri2: ", mop_iri)
                    if mop_iri == []:
                        failed_geometry_entries.append(row['MOP Formula'])
                        print(f"failed for: {row['MOP Formula']}")
                    
                    elif mop_iri != []:
                        geometry_file               = row['Mops_Geometry']
                        print(mop_iri, geometry_file)
                        iri                         = mop_iri[0]
                        self.update_geometry(iri["MOPIRI"], geometry_file[:-4])

            print("failed MOPs: ", failed_geometry_entries)
        


    def update_geometry(self, subject_iri, unique_id):
        # set up string to be stored
        id_hash_value       = str(uuid.uuid4())
        geometry_update     = f"http://68.183.227.15:3838/file-server/{unique_id}.xyz"
        insert_statement    = f"""<{subject_iri}> <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasGeometry> <http://www.theworldavatar.com/kb/ontomops/Geometry_{id_hash_value}> .
                                    <http://www.theworldavatar.com/kb/ontomops/Geometry_{id_hash_value}> <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#hasGeometryFile>   "{geometry_update}" ;
                                                                                                        <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>            <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Geometry> .
                                                        """
        self.query_update(insert_stat=insert_statement, delete_stat="", where_stat="" )
        print(f"Triple added: {insert_statement}")

    def process_results(self, response, file_directory):
        missing_kg                  = []
        for res in response:
            mop_iri                 = res['MOPIRI']
            #ccdc_num                = res['CCDCNum']
            parsed_url = urlparse(res['MOPIRI'])
            # Extract the last part of the path
            ccdc_num = parsed_url.path.split('/')[-1]
            file_path               = os.path.join(file_directory, f"{ccdc_num}.xyz")
            if os.path.isfile(file_path):
                print(f"File found for CCDC number {ccdc_num}: {file_path}")
                self.update_geometry(mop_iri, ccdc_num)
            else:
                print(f"File not found for CCDC number {ccdc_num}: {file_path}")
                missing_kg.append(ccdc_num)
        print("The following CCDC numbers are in the KG but no .xyz file exists: ", missing_kg)
            
    def move_file_to_subfolder(self, source_file, destination_subfolder):
        """
        Moves a file to a specified subfolder.

        :param source_file: The path to the source file that needs to be moved.
        :param destination_subfolder: The path to the subfolder where the file should be moved.
        """
        # Get the directory where the current script is located
        script_dir = os.path.dirname(os.path.realpath(__file__))

        # Construct the absolute paths for the source file and the destination subfolder
        source_file_path = os.path.join(script_dir, source_file)
        destination_subfolder_path = os.path.join(script_dir, destination_subfolder)

        # Ensure the destination subfolder exists
        if not os.path.exists(destination_subfolder_path):
            os.makedirs(destination_subfolder_path)

        # Construct the destination file path
        destination_file_path = os.path.join(destination_subfolder_path, os.path.basename(source_file))

        # Move the file
        shutil.move(source_file_path, destination_file_path)

        print(f"Moved '{source_file_path}' to '{destination_file_path}'")

    def write_xyz_file(self, directory:str) -> None:
        """
        Writes the geometry of chemical building units (CBUs) to files.

        This function queries the geometries associated with unique species in the dataset and writes each geometry 
        to a file. The filename is derived from the unique identifier (CBUIRI) of the chemical building unit, with 
        an extension of .xyz.

        Files are written in the current working directory.
        Args:
            directory (str): The directory where the files should be written.

        """
        where_geom              = f"""  ?CBUIRI		os:hasUniqueSpecies			?SpeciesIRI	.
  		                                ?SpeciesIRI os:hasGeometry				?geom		. """
        select_geom             = """?CBUIRI ?geom"""
        response                = self.query_triple(where_geom, select_geom)
        for geom in response:
            # Parse the URL
            parsed_url = urlparse(geom["CBUIRI"])
            # Extract the last part of the path
            filename = parsed_url.path.split('/')[-1]
            # Write the text to the file
            file_path = os.path.join(directory, f"{filename}.xyz")
            with open(file_path , 'w') as file:
                file.write(geom["geom"])
            print(file)
        return
    
def reverse_mopformula(mopformula):
    pattern = re.compile(r'(\[[^\]]+\])(\d+)(\[[^\]]+\])(\d+)')
    
    # Search for the pattern in the input string
    match = pattern.search(mopformula)
    
    if match:
        # Extract the matched groups
        a = match.group(1)
        x = match.group(2)
        b = match.group(3)
        y = match.group(4)
        
        # Form the new string [b]y[a]x
        transformed_string = f'{b}{y}{a}{x}'
        return transformed_string
    else:
        # If the input string does not match the pattern, return it unchanged
        return mopformula
    
def contains_underscore(input_string):
    return '_' in input_string

def main():

    script_dir = os.path.dirname(os.path.abspath(__file__))
    #   make file path dependent on script location
    a_box_updates_config_mop                = config_a_box_updates(os.path.join(script_dir, "../OntoMOPConnection.env"))
    #   instantiate class
    file_directory                          = os.path.join(script_dir, "../data/NEW_MOPS_Batch_1.csv")
    updater_mop                             = MOPGeometryUpdater(
        query_endpoint                      = a_box_updates_config_mop.SPARQL_QUERY_ENDPOINT,
        update_endpoint                     = a_box_updates_config_mop.SPARQL_UPDATE_ENDPOINT,
        kg_user                             = a_box_updates_config_mop.KG_USERNAME,
        kg_password                         = a_box_updates_config_mop.KG_PASSWORD
    )
    #response                                = updater_mop.query_ccdc_numbers()
    response                                = updater_mop.read_mops_data(file_directory)
    print(response)

    #updater_mop.write_xyz_file(os.path.abspath(os.path.join(script_dir, "../../data/CBU_geometry")))

if __name__ == "__main__":
    main()
