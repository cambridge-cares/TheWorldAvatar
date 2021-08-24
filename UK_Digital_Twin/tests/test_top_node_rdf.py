import unittest
import os
from os import listdir
from os.path import isfile, join

from Top_Node_Generator.topnodeABoxGeneration import generateTopNodeGraph
from tests.test_power_plant_rdf import TestPowerPlantInfoRDF

"""Path to the generated top node represented using RDF"""
path_to_generated_top_node_rdf = '../resources/test_generated_top_node_rdf'
"""Path to the reference electricity consumption data represented using RDF"""
path_to_reference_top_node_rdf = '../resources/test_reference_top_node_rdf'

"""
The goal of this class is to test the semantic representation of the top node of the UK power system.
"""
class TestTopNodeRDF(unittest.TestCase):
    """
    Test that it can generate the same axioms provided in the reference OWL file.
    """
    def test_compare_top_node_axioms(self):
        """
        Gets absolute path from a relative path
        """
        abs_path_generated_top_node_rdf = os.path.abspath(path_to_generated_top_node_rdf)
        abs_path_reference_top_node_rdf = os.path.abspath(path_to_reference_top_node_rdf)
        """
        Calls the function that generates RDF representations of the top node
        """
        generateTopNodeGraph('default', False, abs_path_generated_top_node_rdf, True)
        owl_files = [f for f in listdir(path_to_generated_top_node_rdf) if isfile(join(path_to_generated_top_node_rdf, f))]
        testPowerPlantInfoRDF = TestPowerPlantInfoRDF()
        for an_owl_file in owl_files:
            generated_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_generated_top_node_rdf + os.sep + an_owl_file)
            reference_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_reference_top_node_rdf + os.sep + an_owl_file)
            print('Comparing the content of the following two files:\n', abs_path_generated_top_node_rdf + os.sep + an_owl_file,
                  '\n', abs_path_reference_top_node_rdf + os.sep + an_owl_file)
            self.assertEqual(generated_statements, reference_statements)
            print('The files have the same content.')
            print('\n')

if __name__ == '__main__':
    """
    Allows to run the test case from the command-line interface
    """
    # unittest.main()