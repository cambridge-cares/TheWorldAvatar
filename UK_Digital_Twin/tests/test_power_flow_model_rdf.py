import unittest
import os
from os import listdir
from os.path import isfile, join

from UK_Power_Grid_Model_Generator.model_EBusABoxGeneration import createModel_EBus
from UK_Power_Grid_Model_Generator.model_EGenABoxGeneration import createModel_EGen
from UK_Power_Grid_Model_Generator.model_ELineABoxGeneration import createModel_ELine
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import createTopologyGraph, addEBusNodes, addELineNodes, \
    addEGenNodes
from tests.test_power_plant_rdf import TestPowerPlantInfoRDF

"""Path to the generated power-flow model represented using RDF"""
path_to_generated_power_flow_model_rdf = '../resources/test_generated_power_flow_model_rdf'
"""Path to the reference power-flow model represented using RDF"""
path_to_reference_power_flow_model_rdf = '../resources/test_reference_power_flow_model_rdf'
test_input_folder_name = 9999

"""
The goal of this class is to test the semantic representation of power-flow model.
"""
class TestPowerFlowModelRDF(unittest.TestCase):
    """
    Test that it can generate the same power-flow model axioms provided in the reference OWL files.
    """
    def test_compare_power_grid_topology(self):
        """
        Gets absolute path from a relative path
        """
        abs_path_generated_power_flow_model_rdf = os.path.abspath(path_to_generated_power_flow_model_rdf)
        abs_path_reference_power_flow_model_rdf = os.path.abspath(path_to_reference_power_flow_model_rdf)
        """
        Calls the function that generates RDF representations of lines in the power-flow model
        """
        createModel_ELine('default', False, 9999, 14, 9999, abs_path_generated_power_flow_model_rdf, True)
        """
        Calls the function that generates RDF representations of buses in the power-flow model
        """
        createModel_EBus('default', False, 9999, abs_path_generated_power_flow_model_rdf, True)

        owl_files = [f for f in listdir(path_to_reference_power_flow_model_rdf) if isfile(join(path_to_reference_power_flow_model_rdf, f))]
        testPowerPlantInfoRDF = TestPowerPlantInfoRDF()
        for an_owl_file in owl_files:
            generated_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_generated_power_flow_model_rdf + os.sep + an_owl_file)
            reference_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_reference_power_flow_model_rdf + os.sep + an_owl_file)
            print('Comparing the content of the following two files:\n', abs_path_generated_power_flow_model_rdf + os.sep + an_owl_file,
                  '\n', abs_path_reference_power_flow_model_rdf + os.sep + an_owl_file)
            self.assertEqual(generated_statements, reference_statements)
            print('The files have the same content.')
            print('\n')

if __name__ == '__main__':
    """
    Allows to run the test case from the command-line interface
    """
    # unittest.main()