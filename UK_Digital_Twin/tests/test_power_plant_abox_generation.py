import unittest
import os

path_to_generated_rdf = '../resources/test_generated_rdf'
path_to_reference_rdf = '../resources/test_reference_rdf'
test_input_folder_name = 9999

"""
The goal of this class is to test all major features of the UK Ditial Twin project,
namely, representations of:<br>
i) power plants,
ii) energy consumption, and
iii) power-grid topology.
"""
class TestUKDigitalTwin(unittest.TestCase):
    """
    Test that it can generate the instances provided in the reference OWL files.
    """
    def test_compare_power_plant_rdf_representations(self):
       """
       Gets absolute path from a relative path
       """
       abs_path_generated_rdf = os.path.abspath(path_to_generated_rdf)
       abs_path_reference_rdf = os.path.abspath(path_to_reference_rdf)