import unittest
import os
from os import listdir
from os.path import isfile, join

from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import createTopologyGraph, addEBusNodes, addELineNodes, \
    addEGenNodes
from tests.test_power_plant_rdf import TestPowerPlantInfoRDF

"""Path to the generated power grid topology represented using RDF"""
path_to_generated_power_grid_topology_rdf = '../resources/test_generated_power_grid_topology_rdf'
"""Path to the reference power grid topology represented using RDF"""
path_to_reference_power_grid_topology_rdf = '../resources/test_reference_power_grid_topology_rdf'
test_input_folder_name = 9999

"""
The goal of this class is to test the semantic representation of power grid topology.
"""
class TestPowerGridTopologyRDF(unittest.TestCase):
    """
    Test that it can generate the same power grid topology axioms provided in the reference OWL files.
    """
    def test_compare_power_grid_topology(self):
        """
        Gets absolute path from a relative path
        """
        abs_path_generated_power_grid_topology_rdf = os.path.abspath(path_to_generated_power_grid_topology_rdf)
        abs_path_reference_power_grid_topology_rdf = os.path.abspath(path_to_reference_power_grid_topology_rdf)
        """
        Calls the function that generates RDF representations of buses in power grid topology
        """
        createTopologyGraph('default', False, 9999, 14, addEBusNodes, None, None, path_to_generated_power_grid_topology_rdf, True)
        """
        Calls the function that generates RDF representations of lines in power grid topology
        """
        createTopologyGraph('default', False, 9999, 14, None, addELineNodes, None, path_to_generated_power_grid_topology_rdf, True)
        """
        Calls the function that generates RDF representations of generators in power grid topology
        """
        createTopologyGraph('default', False, 9999, 14, None, None, addEGenNodes, path_to_generated_power_grid_topology_rdf, True)

        owl_files = [f for f in listdir(path_to_generated_power_grid_topology_rdf) if isfile(join(path_to_generated_power_grid_topology_rdf, f))]
        testPowerPlantInfoRDF = TestPowerPlantInfoRDF()
        for an_owl_file in owl_files:
            generated_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_generated_power_grid_topology_rdf + os.sep + an_owl_file)
            reference_statements = testPowerPlantInfoRDF.read_rdf_model_statements(abs_path_reference_power_grid_topology_rdf + os.sep + an_owl_file)
            print('Comparing the content of the following two files:\n', abs_path_generated_power_grid_topology_rdf + os.sep + an_owl_file,
                  '\n', abs_path_reference_power_grid_topology_rdf + os.sep + an_owl_file)
            self.assertEqual(generated_statements, reference_statements)
            print('The files have the same content.')
            print('\n')

if __name__ == '__main__':
    """
    Allows to run the test case from the command-line interface
    """
    # unittest.main()