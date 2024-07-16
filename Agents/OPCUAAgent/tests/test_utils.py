import pytest
import tempfile
import os
import pathlib
from unittest.mock import patch

from OPCUAAgent import utils

class Test_utils:
    def setUp(self):
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test.properties',
                                              'dbname=test')
    
    def _create_temporary_file_with_data(self, file_path, content):
        with open(file_path, 'w') as ifile:
            ifile.write(content)

    def tearDown(self):
        self._temp_dir.cleanup()
        
    #Set up environment variable with path
    @patch.dict(os.environ, {'TEST_GET_ENV': "correct"})
    #Test successful utils.get_env_variable
    def test_get_env_variable_success(self):
        self.setUp()
        assert utils.get_env_variable("TEST_GET_ENV") == "correct"
        self.tearDown()
    
    #Remove all environment variables
    @patch.dict(os.environ, {}, clear=True) 
    #Test failed utils.get_env_variable
    def test_get_env_variable_fail(self):
        with pytest.raises(KeyError) as excinfo:
            filePath = utils.get_env_variable("NON_EXISTENT_ENV")
        # Check correct exception message
        assert 'Environment variable \'NON_EXISTENT_ENV\' not found.' in str(excinfo.value)
        
    #Test successful utils.read_property
    def test_read_property_success(self):
        self.setUp()
        assert utils.read_property(str(self.temp_path / 'test.properties'), "dbname") == "test"
        self.tearDown()
    
    #Test failed utils.read_property
    def test_read_property_fail(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            value = utils.read_property(str(self.temp_path / 'test.properties'), "non_existent_key")
            assert 'Key \'non_existent_key\' not found.' in str(excinfo.value)
        self.tearDown()
        
# To invoke the pytest framework and run all tests
if __name__ == "__main__":
  pytest.main()