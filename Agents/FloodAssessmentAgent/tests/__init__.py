# ----------------------------------------------------------------------------------
# This file mocks all module imports which depends on Stack deployment
# Without this, creating the agent app will throw exceptions due to missing variables/imports
#
# On import of `tests` package's __init__.py, available objects for import can be replaced
# in sys.modules, i.e. the updated objects will be returned by import statements:
#
# Every import of `floodassessment.utils.env_configs` will now obtain a reference to 
# `floodassessment.tests.mockutils.env_configs_mock`. Since the import machinery already finds
# the keys in the sys.modules dictionary, it will not begin looking for the old files.
#
# ----------------------------------------------------------------------------------
from . import mockutils
from .mockutils import env_configs_mock, stack_configs_mock
import sys

sys.modules["floodassessment.utils"] = mockutils
sys.modules['floodassessment.utils.env_configs'] = env_configs_mock
sys.modules['floodassessment.utils.stack_configs'] = stack_configs_mock
