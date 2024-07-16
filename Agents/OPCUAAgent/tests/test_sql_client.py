import pytest
import tempfile
import os
import pathlib
from unittest.mock import patch
import time

from OPCUAAgent import sql_client

class Test_sql_client:
    def setUp(self):
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test_conf.properties',
                                              'dbname=postgres\n' +
                                              'user=postgres\n' +
                                              'password=postgres\n' +
                                              'host=postgres_agent_test\n' +
                                              'port=5432\n')
        
    def setUp_fail(self):
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test_conf.properties',
                                              'dbname=non_existent_db\n' +
                                              'user=postgres\n' +
                                              'password=postgres\n' +
                                              'host=postgres_agent_test\n' +
                                              'port=5432\n')
    
    def _create_temporary_file_with_data(self, file_path, content):
        with open(file_path, 'w') as ifile:
            ifile.write(content)

    def tearDown(self):
        self._temp_dir.cleanup()
          
    def test_connect_to_database_fail(self):
        self.setUp_fail()
        #sleep test to wait for postgresql container to spin up
        time.sleep(5)
        with patch.dict(os.environ, {"POSTGRES_CONF":str(self.temp_path / 'test_conf.properties')}, clear=True):
            with pytest.raises(Exception) as excinfo:
                sql_client.connect_to_database()
            assert "FATAL:  database \"non_existent_db\" does not exist" in str(excinfo.value)
        self.tearDown()