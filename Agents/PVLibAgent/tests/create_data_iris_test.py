# The purpose of this module is to test create_data_iris.py
# ===============================================================================

import pytest
from configobj import ConfigObj

from PVLibAgent.data_instantiation.create_data_iris import check_data_iris
import pathlib
import tempfile


class TestCreateDataIris:

    def setUp(self):
        """ Called before every test. """
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test_dataIRIs.properties',
                                              '')


    def tearDown(self):
        """ Called after every test. """
        self._temp_dir.cleanup()

    def _create_temporary_file_with_data(self, file_path, content):
        with open(file_path, 'w') as ifile:
            ifile.write(content)

    def test_missing_keys(self):
        self.setUp()
        # test missing AC_Power key
        with pytest.raises(KeyError) as excinfo:
            check_data_iris.check_data_iris_and_create_if_not_exist(str(self.temp_path / 'test_dataIRIs.properties'))
        # Check correct exception message
        assert 'Key "AC_Power" is missing in properties file: ' in str(excinfo.value)

        # test missing DC_Power key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_dataIRIs.properties',
                                                  "AC_Power=test")
            check_data_iris.check_data_iris_and_create_if_not_exist(str(self.temp_path / 'test_dataIRIs.properties'))
        # Check correct exception message
        assert 'Key "DC_Power" is missing in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_no_values_provided(self):
        self.setUp()
        # test no AC_Power value
        self._create_temporary_file_with_data(self.temp_path / 'test_dataIRIs.properties',
                                                  "AC_Power=" + "\n" + "DC_Power=test")
        check_data_iris.check_data_iris_and_create_if_not_exist(str(self.temp_path / 'test_dataIRIs.properties'))
        props = ConfigObj(str(self.temp_path / 'test_dataIRIs.properties'))
        ac_power = props['AC_Power']
        assert ac_power != ''
        assert ac_power.__contains__('https://www.theworldavatar.com/kg/ontotimeseries/pvlib_ac_power')

        # test no DC_Power value
        self._create_temporary_file_with_data(self.temp_path / 'test_dataIRIs.properties',
                                                  "AC_Power=test" + "\n" + "DC_Power=")
        check_data_iris.check_data_iris_and_create_if_not_exist(str(self.temp_path / 'test_dataIRIs.properties'))
        props = ConfigObj(str(self.temp_path / 'test_dataIRIs.properties'))
        dc_power = props['DC_Power']
        assert dc_power != ''
        assert dc_power.__contains__('https://www.theworldavatar.com/kg/ontotimeseries/pvlib_dc_power')

        # test no DC_Power and AC_Power value
        self._create_temporary_file_with_data(self.temp_path / 'test_dataIRIs.properties',
                                              "AC_Power=" + "\n" + "DC_Power=")
        check_data_iris.check_data_iris_and_create_if_not_exist(str(self.temp_path / 'test_dataIRIs.properties'))
        props = ConfigObj(str(self.temp_path / 'test_dataIRIs.properties'))
        ac_power = props['AC_Power']
        assert ac_power != ''
        assert ac_power.__contains__('https://www.theworldavatar.com/kg/ontotimeseries/pvlib_ac_power')
        assert dc_power != ''
        assert dc_power.__contains__('https://www.theworldavatar.com/kg/ontotimeseries/pvlib_dc_power')
        self.tearDown()


