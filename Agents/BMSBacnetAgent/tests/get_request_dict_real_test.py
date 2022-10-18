import unittest

from unittest.mock import patch,Mock
from bacnet.request_dict_formatter import getRequestDict
from deviceMap.bms_bacnet_dictionary import bmsBacnetDict


class TestRequestDictFormatter(unittest.TestCase):


    def test_get_getRequestDict(self):
        bms_bacnet_dict = bmsBacnetDict().build() #
        device_list, req_dicts = getRequestDict(bms_bacnet_dict)
        print(device_list)
        print(req_dicts)


if __name__ == '__main__':
    unittest.main()