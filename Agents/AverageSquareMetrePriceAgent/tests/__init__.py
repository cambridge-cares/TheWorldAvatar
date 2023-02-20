# ----------------------------------------------------------------------------------
# Mock importing modules, which depend on Stack deployment
#
# On import of `tests` package's __init__.py, available objects for import can be replaced
# in sys.modules, i.e. the updated objects will be returned by import statements:
#
# Every import of `avgsqmpriceagent.utils.env_configs` will now obtain a reference to 
# `avgsqmpriceagent.mockutils.env_configs_mock`. Since the import machinery already finds
# the keys in the sys.modules dictionary, it will not begin looking for the old files.
#
#
# ----------------------------------------------------------------------------------
from . import mockutils
from .mockutils import env_configs_mock, stack_configs_mock
import sys

sys.modules["avgsqmpriceagent.utils"] = mockutils
sys.modules['avgsqmpriceagent.utils.env_configs'] = env_configs_mock
sys.modules['avgsqmpriceagent.utils.stack_configs'] = stack_configs_mock
