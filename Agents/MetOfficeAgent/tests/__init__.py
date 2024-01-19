# ----------------------------------------------------------------------------------
# Mock importing modules, which depend on Stack deployment
#
# On import of `tests` package's __init__.py, available objects for import can be replaced
# in sys.modules, i.e. the updated objects will be returned by import statements:
#
# Every import of `agent.utils.env_configs` will now obtain a reference to 
# `agent.mockutils.env_configs_mock`. Since the import machinery already finds
# the keys in the sys.modules dictionary, it will not begin looking for the old files.
#
# -> Mock all modules, which depend on Stack deployment and/or run of Flask App startup
# ----------------------------------------------------------------------------------

import sys
from .mockutils import env_configs_mock, stack_configs_mock, initialise_kg_mock

sys.modules['agent.utils.env_configs'] = env_configs_mock
sys.modules['agent.utils.stack_configs'] = stack_configs_mock
sys.modules['agent.kgutils.initialise_kg'] = initialise_kg_mock
