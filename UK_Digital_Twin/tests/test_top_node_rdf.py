import unittest
import os

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

if __name__ == '__main__':
    """
    Allows to run the test case from the command-line interface
    """
    # unittest.main()