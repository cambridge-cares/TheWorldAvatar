from pesfit.app import pesfit_wrapper
import pytest
import os
import json

TEST_DIR = os.path.dirname(os.path.abspath(__file__))

@pytest.mark.parametrize("inputs", \
[
(   "http://www.theworldavatar.com/kb/ontopesscan/PotentialEnergySurfaceScan_b6d609c6-fe80-4bb8-aaf7-d4675ffd2638", \
    "configuration_example.json")
]
)
def test_pesfit(inputs):
    print('========================================================')
    print('TEST INPUT FILE: ', inputs)
    print()
    print()

    pesfit_wrapper(inputs)