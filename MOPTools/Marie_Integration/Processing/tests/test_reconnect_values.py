import unittest
from unittest.mock import patch, MagicMock
from Reconnect import Reconnect

class TestReconnect(unittest.TestCase):

    @patch('Reconnect.UpdateKG.query_triple')
    @patch('Reconnect.UpdateKG.generate_triple')
    @patch('Reconnect.UpdateKG.delete_triple')
    def test_reconnect_values(self, mock_delete_triple, mock_generate_triple, mock_query_triple):
        # Mock response
        mock_query_triple.return_value = [
            {"AMIri": "http://example.org/AMI_001", "geom": "geometry_value_1"},
            {"AMIri": "http://example.org/AMI_002", "geom": "geometry_value_2"},
        ]

        updater = Reconnect('http://example.org/sparql', 'http://example.org/update', 'user', 'password')
        updater.reconnect_values()

        # Verify method calls
        assert mock_generate_triple.call_count == 2
        assert mock_delete_triple.call_count == 2

        mock_generate_triple.assert_any_call("http://example.org/AMI_001", "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value", "geometry_value_1", literal=True)
        mock_generate_triple.assert_any_call("http://example.org/AMI_002", "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value", "geometry_value_2", literal=True)

        mock_delete_triple.assert_any_call("http://example.org/AMI_001", "http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#value", "geometry_value_1", literal=True)
        mock_delete_triple.assert_any_call("http://example.org/AMI_002", "http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#value", "geometry_value_2", literal=True)

if __name__ == '__main__':
    unittest.main()
