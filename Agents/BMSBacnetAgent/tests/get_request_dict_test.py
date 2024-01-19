import unittest

from unittest.mock import patch,Mock
from bacnet.request_dict_formatter import getRequestDict

propIRI = "http://www.theworldavatar.com/VAV-E7-28_DS_sensor1.owl#V_DamperStateOfVAV-E/7-28"
deviceIRI = "http://www.theworldavatar.com/VAV-E7-28_DS_sensor1.owl#VAV-E/7-28_DS_sensor1"

class TestRequestDictFormatter(unittest.TestCase):


    def test_get_getRequestDict(self):
        mock_bmsBacnetDict = Mock()
        mock_bmsBacnetDict.build.return_value = {
            deviceIRI: 111
        }
        mockBmsPropDict = Mock()
        mockBmsPropDict.build.return_value = {
            deviceIRI: [1111]
        }
        device_list, req_dicts = getRequestDict(mock_bmsBacnetDict.build(), mockBmsPropDict.build())
        print(device_list)
        print(req_dicts)


if __name__ == '__main__':
    unittest.main()