import unittest

from agents.bms_bacnet_agent import bmsBacnetAgent
from unittest.mock import patch, Mock
deviceIRI = "http://www.theworldavatar.com/VAV-E7-28_DS_sensor1.owl#VAV-E/7-28_DS_sensor1"

class TestAgent(unittest.TestCase):
    @patch('agents.bms_bacnet_agent.bmsBacnetDict')
    @patch('agents.bms_bacnet_agent.bmsPropMap')
    @patch('agents.bms_bacnet_agent.Connection')
    def test_update_bms(self, mockConnection, mockBmsPropDict, mock_bmsBacnetDict ):
        mock_bmsBacnetDict.return_value.build.return_value = {deviceIRI: 111}
        mockBmsPropDict.return_value.build.return_value = {deviceIRI: [1111]}
        mockConnection.return_value.readMultiple.return_value = [37.44]
        agent = bmsBacnetAgent()
        agent.updateKB()


if __name__ == '__main__':
    unittest.main()