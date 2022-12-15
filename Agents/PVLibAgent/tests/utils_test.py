# The purpose of this module is to test utils.py
# ===============================================================================

import pytest
from PVLibAgent.kg_utils.utils import read_properties_file, create_sparql_prefix, PREFIXES
import pathlib
import tempfile


class TestUtils:
    def setUp(self):
        """ Called before every test. """
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                              '')


    def tearDown(self):
        """ Called after every test. """
        self._temp_dir.cleanup()

    def _create_temporary_file_with_data(self, file_path, content):
        with open(file_path, 'w') as ifile:
            ifile.write(content)

    def test_missing_sparql_query_endpoint(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "sparql.query.endpoint" is missing' in str(excinfo.value)

        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=")
            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'No "sparql.query.endpoint" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_missing_sparql_update_endpoint(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123")
            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "sparql.update.endpoint" is missing' in str(excinfo.value)

        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=")
            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
            # Check correct exception message
        assert 'No "sparql.update.endpoint" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_missing_db_query_url(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123")
            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "db.query.url" is missing' in str(excinfo.value)

        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=")
            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
            # Check correct exception message
        assert 'No "db.query.url" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_missing_db_query_user(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "db.query.user" is missing' in str(excinfo.value)

        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=")
            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
            # Check correct exception message
        assert 'No "db.query.user" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_missing_db_query_password(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "db.query.password" is missing' in str(excinfo.value)

        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=")
            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
            # Check correct exception message
        assert 'No "db.query.password" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_missing_db_update_url(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "db.update.url" is missing' in str(excinfo.value)

        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test" + "\n" + "db.update.url=")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'No "db.update.url" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_missing_db_update_user(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test"+ "\n" + "db.update.url=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "db.update.user" is missing' in str(excinfo.value)

        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test" + "\n" + "db.update.url=test" + "\n" +
                                                  "db.update.user=")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'No "db.update.user" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_missing_db_update_password(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test" + "\n" + "db.update.url=test" + "\n" + "db.update.user=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "db.update.password" is missing' in str(excinfo.value)

        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test" + "\n" + "db.update.url=test" + "\n" +
                                                  "db.update.user=test" + "\n" + "db.update.password=")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'No "db.update.password" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_missing_iri(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test" + "\n" + "db.update.url=test" + "\n" +
                                                  "db.update.user=test" + "\n" + "db.update.password=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "iri" is missing' in str(excinfo.value)
        self.tearDown()

    def test_missing_air_temperature_iri(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test" + "\n" + "db.update.url=test" + "\n" +
                                                  "db.update.user=test" + "\n" + "db.update.password=test" + "\n" +
                                                  "iri=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "air.temperature.iri" is missing' in str(excinfo.value)
        self.tearDown()

    def test_missing_wind_speed_iri(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test" + "\n" + "db.update.url=test" + "\n" +
                                                  "db.update.user=test" + "\n" + "db.update.password=test" + "\n" +
                                                  "iri=test" + "\n" + "air.temperature.iri=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "wind.speed.iri" is missing' in str(excinfo.value)
        self.tearDown()

    def test_missing_irradiance_iri(self):
        self.setUp()
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_ts_client.properties',
                                                  "sparql.query.endpoint=123" + "\n" + "sparql.update.endpoint=123" +
                                                  "\n" + "db.query.url=test" + "\n" + "db.query.user=test" + "\n" +
                                                  "db.query.password=test" + "\n" + "db.update.url=test" + "\n" +
                                                  "db.update.user=test" + "\n" + "db.update.password=test" + "\n" +
                                                  "iri=test" + "\n" + "air.temperature.iri=test" + "\n" +
                                                  "wind.speed.iri=test")

            read_properties_file(str(self.temp_path / 'test_ts_client.properties'))
        # Check correct exception message
        assert 'Key "irradiance.iri" is missing' in str(excinfo.value)
        self.tearDown()

    def test_create_sparql_prefixes(self):
        self.setUp()
        # test non-existing prefix "random" in PREFIXES
        with pytest.raises(KeyError) as excinfo:
            create_sparql_prefix('random')
        assert 'Prefix: "random" has not been specified' in str(excinfo.value)
        
        string = create_sparql_prefix('ontoems')
        assert 'ontoems: <https://www.theworldavatar.com/kg/ontoems/>' in string
        self.tearDown()
