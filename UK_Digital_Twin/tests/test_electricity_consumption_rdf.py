import unittest
import os
from os import listdir
from os.path import isfile, join

from UK_Energy_Consumption_Generator.energyConsumptionABoxGeneration import addUKElectricityConsumptionTriples
from tests.test_power_plant_rdf import TestPowerPlantInfoRDF

"""Path to the generated electricity consumption data represented using RDF"""
path_to_generated_elec_consumption_rdf = '../resources/test_generated_elec_consumption_rdf'
"""Path to the reference electricity consumption data represented using RDF"""
path_to_reference_elec_consumption_rdf = '../resources/test_reference_elec_consumption_rdf'
test_input_folder_name = 9999

"""
The goal of this class is to test the semantic representation of electric energy consumption data.
"""
class TestElectricityConsumptionRDF(unittest.TestCase):
    """
    Test that it can generate the same electricity consumption data axioms provided in the reference OWL files.
    """
    def test_compare_electricity_consumption_facts(self):
        """
        Gets absolute path from a relative path
        """
        abs_path_generated_elec_consumption_rdf = os.path.abspath(path_to_generated_elec_consumption_rdf)
        abs_path_reference_elec_consumption_rdf = os.path.abspath(path_to_reference_elec_consumption_rdf)
        """
        Calls the function that generates RDF representations of electric energy consumption
        """
        addUKElectricityConsumptionTriples('default', test_input_folder_name, abs_path_generated_elec_consumption_rdf, True)
        owl_files = [f for f in listdir(path_to_generated_elec_consumption_rdf) if isfile(join(path_to_generated_elec_consumption_rdf, f))]
        testPowerPlantInfoRDF = TestPowerPlantInfoRDF()
        for an_owl_file in owl_files:
            generated_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_generated_elec_consumption_rdf + os.sep + an_owl_file)
            reference_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_reference_elec_consumption_rdf + os.sep + an_owl_file)
            print('Comparing the content of the following two files:\n', abs_path_generated_elec_consumption_rdf + os.sep + an_owl_file,
                  '\n', abs_path_reference_elec_consumption_rdf + os.sep + an_owl_file)
            self.assertEqual(generated_statements, reference_statements)
            print('The files have the same content.')
            print('\n')

if __name__ == '__main__':
    """
    Allows to run the test case from the command-line interface
    """
    # unittest.main()