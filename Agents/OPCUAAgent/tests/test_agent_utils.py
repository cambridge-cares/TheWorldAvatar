import pytest
import tempfile
import os
import pathlib
import logging
from unittest.mock import patch

from OPCUAAgent import agent_utils

class Test_agent_utils:
    # set up temp properties file with some key:value
    def setUp(self):
        logging.info("Set up temp properties file with the following content: dbname=test")
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test.properties',
                                              'dbname=test')
        
    # write data to temp file
    def _create_temporary_file_with_data(self, file_path, content):
        with open(file_path, 'w') as ifile:
            ifile.write(content)
            
    # clean up temp directory and files
    def tearDown(self):
        self._temp_dir.cleanup()
        
    #Set up mock environment variable
    @patch.dict(os.environ, {'TEST_GET_ENV': "correct"})
    # test get_env_variable with a properly configured environment variable
    def test_get_env_variable_success(self):
        self.setUp()
        logging.info("Test get_env_variable with a properly configured environment variable")
        assert agent_utils.get_env_variable("TEST_GET_ENV") == "correct"
        self.tearDown()
    
    #Remove all environment variables
    @patch.dict(os.environ, {}, clear=True) 
    # test get_env_variable with an incorrectly configured environment variable
    def test_get_env_variable_fail(self):
        logging.info("Test get_env_variable with an incorrectly configured environment variable, this should raise a KeyError")
        with pytest.raises(KeyError) as excinfo:
            filePath = agent_utils.get_env_variable("NON_EXISTENT_ENV")
        # Check correct exception message
        assert 'Environment variable \'NON_EXISTENT_ENV\' not found.' in str(excinfo.value)
        
    # test read_property with temp properties file and check whether received value is correct
    def test_read_property_success(self):
        self.setUp()
        logging.info("Test read_property with temp properties file and check whether received value is correct")
        assert agent_utils.read_property(str(self.temp_path / 'test.properties'), "dbname") == "test"
        self.tearDown()
    
    # test read_property with temp properties file and an incorrect key
    def test_read_property_fail(self):
        self.setUp()
        logging.info("Test read_property with temp properties file and an incorrect key, this should raise a KeyError")
        with pytest.raises(KeyError) as excinfo:
            value = agent_utils.read_property(str(self.temp_path / 'test.properties'), "non_existent_key")
        assert 'Key \'non_existent_key\' not found.' in str(excinfo.value)
        self.tearDown()
        
    # test remove_char_between_characters
    def test_remove_char_between_characters(self):
        logging.info("Test remove_char_between_characters")
        assert 'testing' == agent_utils.remove_char_between_characters("testing [remove_this]", "[", "]")
        