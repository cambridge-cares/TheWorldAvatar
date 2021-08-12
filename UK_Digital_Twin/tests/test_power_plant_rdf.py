import unittest
import os
from os import listdir
from os.path import isfile, join
from rdflib import Graph
from UK_Power_Plant_Generator.powerPlantABoxGeneration import addUKPowerPlantTriples

path_to_generated_rdf = '../resources/test_generated_power_plant_rdf'
path_to_reference_rdf = '../resources/test_reference_power_plant_rdf'
test_input_folder_name = 9999

"""
The goal of this class is to test the semantic representation of power plants data and metadata.
"""
class TestPowerPlantInfoRDF(unittest.TestCase):
    """
    Test that it can generate the same instances provided in the reference OWL files.
    """
    def test_compare_power_plant_rdf_representations(self):
        """
        Gets absolute path from a relative path
        """
        abs_path_generated_rdf = os.path.abspath(path_to_generated_rdf)
        abs_path_reference_rdf = os.path.abspath(path_to_reference_rdf)
        """
        Calls the function that generates RDF representations of power plants
        """
        addUKPowerPlantTriples('default', test_input_folder_name, abs_path_generated_rdf)
        owl_files = [f for f in listdir(path_to_generated_rdf) if isfile(join(path_to_generated_rdf, f))]
        testPowerPlantInfoRDF = TestPowerPlantInfoRDF()
        for an_owl_file in owl_files:
            generated_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_generated_rdf + os.sep + an_owl_file)
            reference_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_reference_rdf + os.sep + an_owl_file)
            print('Comparing the content of the following two files:\n', abs_path_generated_rdf + os.sep + an_owl_file,
                  '\n', abs_path_reference_rdf + os.sep + an_owl_file)
            self.assertEqual(generated_statements, reference_statements)
            print('The files have the same content.')
            print('\n')

    """
    Reads an RDF model to return all the statements codified it
    """
    def read_rdf_model_statements(self, path):
        g = Graph()
        g.parse(path)
        stmts = set()
        for stmt in g:
            stmts.add(stmt)
        return stmts

if __name__ == '__main__':
    """
    Allows to run the test case from the command-line interface
    """
    unittest.main()