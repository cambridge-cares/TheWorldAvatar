'''
A mock test provided to check the BACNET API.
'''

import unittest

from bacnet.connection import Connection
from unittest.mock import patch, Mock
deviceIRI = "http://www.theworldavatar.com/VAV-E7-28_DS_sensor1.owl#VAV-E/7-28_DS_sensor1"

class TestAgent(unittest.TestCase):
    @patch('bacnet.connection.BAC0.connect')
    def test_mock_bacnet(self, mockBCN):
        mockBCN.return_value.readMultiple.return_value = {
        ('analogInput', 1111): [
        ('presentValue', 37.4429),
        ('units', 'pascal'),
        ]}
        dummyConnection= Connection()
        dummyConnection.connect(0)
        extractedD = dummyConnection.readMultiple("", {})
        print(extractedD)
        self.assertEqual(extractedD, [37.4429])


if __name__ == '__main__':
    unittest.main()