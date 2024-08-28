from datetime import datetime, timezone
import datetime
import pytest
import tempfile
import os
import pathlib
from unittest.mock import patch
import time
import logging
from psycopg2 import sql

from OPCUAAgent import sql_client

class Test_sql_client:
    # set up temp properties file with default postgresql information
    def setUp(self):
        logging.info("Set up temp properties file with default postgresql information")
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test_conf.properties',
                                              'dbname=postgres\n' +
                                              'user=postgres\n' +
                                              'password=postgres\n' +
                                              'host=postgres_test\n' +
                                              'port=5432\n')
        
    # set up temp properties file with postgresql information and non existent db name
    def setUp_fail_non_existent_db(self):
        logging.info("Set up temp properties file with postgresql information and non existent db name")
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test_conf.properties',
                                              'dbname=non_existent_db\n' +
                                              'user=postgres\n' +
                                              'password=postgres\n' +
                                              'host=postgres_test\n' +
                                              'port=5432\n')
        
    # set up temp properties file with postgresql information and new db called test
    def setUp_new_db(self):
        logging.info("Set up temp properties file with postgresql information and new db called test")
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test_conf.properties',
                                              'dbname=test\n' +
                                              'user=postgres\n' +
                                              'password=postgres\n' +
                                              'host=postgres_test\n' +
                                              'port=5432\n')
        
    # write data to temp file
    def _create_temporary_file_with_data(self, file_path, content):
        with open(file_path, 'w') as ifile:
            ifile.write(content)
            
    # clean up temp directory and files
    def tearDown(self):
        self._temp_dir.cleanup()
        
    # test create_database_if_not_exist without environment variable POSTGRES_CONF
    def test_create_database_if_not_exist_fail(self):
        self.setUp_new_db()
        logging.info("Test create_database_if_not_exist without environment variable POSTGRES_CONF, this should raise an exception")
        #sleep test to wait for postgresql container to spin up
        time.sleep(3)
        with pytest.raises(Exception) as excinfo:
            sql_client.create_database_if_not_exist()
        assert "Error while connecting to PostgreSQL, do check the environment variable and properties file..." in str(excinfo.value)
        self.tearDown()
        
    # test create_database_if_not_exist with environment variable POSTGRES_CONF pointing to the temp file containing postgresql information
    def test_create_database_if_not_exist_success(self):
        self.setUp_new_db()
        logging.info("Test create_database_if_not_exist with environment variable POSTGRES_CONF pointing to the temp properties file")
        #sleep test to wait for postgresql container to spin up
        time.sleep(3)
        with patch.dict(os.environ, {"POSTGRES_CONF":str(self.temp_path / 'test_conf.properties')}, clear=True):
            sql_client.create_database_if_not_exist()
            connection = sql_client.connect_to_database()
        assert connection != None
        assert "test" == connection.info.dbname
        
    # test connect_to_database with non-existent database
    def test_connect_to_database_fail(self):
        self.setUp_fail_non_existent_db()
        logging.info("Test connect_to_database with non-existent database, this should raise an exception")
        #sleep test to wait for postgresql container to spin up
        time.sleep(3)
        with patch.dict(os.environ, {"POSTGRES_CONF":str(self.temp_path / 'test_conf.properties')}, clear=True):
            with pytest.raises(Exception) as excinfo:
                sql_client.connect_to_database()
            assert "FATAL:  database \"non_existent_db\" does not exist" in str(excinfo.value)
        self.tearDown()
        
    # test connect_to_database with default database
    def test_connect_to_database_success(self):
        self.setUp()
        logging.info("Test connect_to_database with default database")
        #sleep test to wait for postgresql container to spin up
        time.sleep(3)
        with patch.dict(os.environ, {"POSTGRES_CONF":str(self.temp_path / 'test_conf.properties')}, clear=True):
            connection = sql_client.connect_to_database()
        assert connection != None
        assert "postgres" == connection.info.dbname
        self.tearDown()
        
    # test create_if_not_exist_and_insert by inserting a mock timeseries data into the database and query the database to check whether the timeseries data exist
    def test_create_if_not_exist_and_insert(self):
        self.setUp_new_db()
        logging.info("Test create_if_not_exist_and_insert by:\n" +
                     "creating a new database called \"test\"\n" +
                     "inserting a mock timeseries data into the database and query the database to check whether the timeseries data exist")
        time.sleep(3)
        with patch.dict(os.environ, {"POSTGRES_CONF":str(self.temp_path / 'test_conf.properties')}, clear=True):
            sql_client.create_database_if_not_exist()
            connection = sql_client.connect_to_database()
            timestamp = datetime.datetime.now(timezone.utc).isoformat(timespec='seconds')
            values_dict = {"testing":{"tag_01":{"timestamp":timestamp, "value":123, "data_type":"Float"}, "tag_02":{"timestamp":timestamp, "value":12, "data_type":"Float"}}}
            sql_client.create_if_not_exist_and_insert(connection, values_dict)
        
        cursor = connection.cursor()
        cursor.execute(
                    sql.SQL("SELECT * FROM {}.{} WHERE timestamp = %s").format(
                    sql.Identifier("opcua_pips"), sql.Identifier("testing")), (timestamp,))
        existing_record = cursor.fetchall()
        assert 1 == len(existing_record)
        self.tearDown()
    
