import os

import pytest

from PVLibAgent.error_handling.exceptions import KGException, SolarModelException
from PVLibAgent.solar_model import SolarModel
import pathlib
import tempfile
from datetime import datetime, timezone

class TestSolarModel:

    def setUp(self):
        """ Called before every test. """
        self._temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = pathlib.Path(self._temp_dir.name)
        self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties', '')

    def tearDown(self):
        """ Called after every test. """
        self._temp_dir.cleanup()

    def _create_temporary_file_with_data(self, file_path, content):
        with open(file_path, 'w') as ifile:
            ifile.write(content)

    def utcformat(self, dt, timespec):
            """convert datetime to string in UTC format (YYYY-mm-ddTHH:MM:SS.mmmZ)"""
            iso_str = dt.astimezone(timezone.utc).isoformat('T', timespec)
            return iso_str.replace('+00:00', 'Z')

    def test_init_read_solar_model_properties(self):
        self.setUp()
        # test missing temp_model key
        with pytest.raises(KeyError) as excinfo:
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "temp_model" is missing in properties file: ' in str(excinfo.value)

        # test missing temp_model value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "temp_model" value has been provided in properties file: ' in str(excinfo.value)

        # test missing temp_model_config key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "temp_model_config" is missing in properties file: ' in str(excinfo.value)

        # test missing temp_model_config value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "temp_model_config" value has been provided in properties file: ' in str(excinfo.value)

        # test missing latitude key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" +"\n" + "temp_model_config=test")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "latitude" is missing in properties file: ' in str(excinfo.value)

        # test missing latitude value and query fails
        with pytest.raises(KGException) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" +"\n" +
                                                  "latitude=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'SPARQL query for latitude not successful.' in str(excinfo.value)

        # test missing longitude key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" +"\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "longitude" is missing in properties file: ' in str(excinfo.value)

        # test missing longitude value and query fails
        with pytest.raises(KGException) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" +"\n" +
                                                  "latitude=10" + "\n" + "longitude=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'SPARQL query for longitude not successful.' in str(excinfo.value)

        # test missing altitude key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" +"\n" +
                                                  "latitude=10" + "\n" + "longitude=10")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "altitude" is missing in properties file: ' in str(excinfo.value)

        # test missing altitude value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" +"\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "altitude" value has been provided in properties file: ' in str(excinfo.value)

        # test missing surface_tilt key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" +"\n" +
                                                  "latitude=10" + "\n" + "longitude=10" +"\n" + "altitude=0")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "surface_tilt" is missing in properties file: ' in str(excinfo.value)

        # test missing surface_tilt value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "surface_tilt" value has been provided in properties file: ' in str(excinfo.value)

        # test missing surface_azimuth key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" +"\n" +
                                                  "surface_tilt=0")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "surface_azimuth" is missing in properties file: ' in str(excinfo.value)

        # test missing surface_azimuth value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "surface_azimuth" value has been provided in properties file: ' in str(excinfo.value)

       # test missing module-rated_dc_power key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" +"\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "module_rated_dc_power" is missing in properties file: ' in str(excinfo.value)

        # test missing module_rated_dc_power value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "module_rated_dc_power" value has been provided in properties file: ' in str(excinfo.value)

        # test missing module_temperature_coefficient key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "module_temperature_coefficient" is missing in properties file: ' in str(excinfo.value)

        # test missing module_temperature_coefficient value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10" + "\n" + "module_temperature_coefficient=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "module_temperature_coefficient" value has been provided in properties file: ' in str(excinfo.value)

        # test missing inverter_rated_dc_power key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10" + "\n" + "module_temperature_coefficient=1")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "inverter_rated_dc_power" is missing in properties file: ' in str(excinfo.value)

        # test missing inverter_rated_dc_power value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10" + "\n" + "module_temperature_coefficient=1"
                                                  + "\n" + "inverter_rated_dc_power=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "inverter_rated_dc_power" value has been provided in properties file: ' in str(excinfo.value)

        # test missing modules_per_string key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10" + "\n" + "module_temperature_coefficient=1"
                                                  + "\n" + "inverter_rated_dc_power=20")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "modules_per_string" is missing in properties file: ' in str(excinfo.value)

        # test missing modules_per_string value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10" + "\n" + "module_temperature_coefficient=1"
                                                  + "\n" + "inverter_rated_dc_power=20" + "\n" + "modules_per_string=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "modules_per_string" value has been provided in properties file: ' in str(excinfo.value)

        # test missing strings_per_inverter key
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10" + "\n" + "module_temperature_coefficient=1"
                                                  + "\n" + "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Key "strings_per_inverter" is missing in properties file: ' in str(excinfo.value)

        # test missing strings_per_inverter value
        with pytest.raises(KeyError) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10" + "\n" + "module_temperature_coefficient=1"
                                                  + "\n" + "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                  + "\n" + "strings_per_inverter=")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'No "strings_per_inverter" value has been provided in properties file: ' in str(excinfo.value)
        self.tearDown()

    def test_init_incorrect_temp_model_parameters(self):
        self.setUp()
        with pytest.raises(SolarModelException) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=test" + "\n" + "temp_model_config=test" + "\n" +
                                                  "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" + "\n" +
                                                  "surface_tilt=0" + "\n" + "surface_azimuth=0" + "\n" +
                                                  "module_rated_dc_power=10" + "\n" + "module_temperature_coefficient=1"
                                                  + "\n" + "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                  + "\n" + "strings_per_inverter=1")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Input parameters for constructing the temperature_model_parameters are incorrect' in str(excinfo.value)
        self.tearDown()

    def test_init_incorrect_PVSystem_parameters(self):
        self.setUp()
        with pytest.raises(SolarModelException) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                  "\n" + "latitude=10" + "\n" + "longitude=10" + "\n" + "altitude=0" +
                                                  "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=wrong_value" + "\n"
                                                  + "module_rated_dc_power=10" + "\n" +
                                                  "module_temperature_coefficient=1" + "\n" +
                                                  "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                  + "\n" + "strings_per_inverter=1")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Input parameters for PVSystem seems to be incorrect.' in str(excinfo.value)
        self.tearDown()

    def test_init_incorrect_location_parameters(self):
        self.setUp()
        with pytest.raises(SolarModelException) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                  "\n" + "latitude=some_string" + "\n" + "longitude=wrong_value" + "\n"
                                                  + "altitude=0" + "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=0"
                                                  + "\n" + "module_rated_dc_power=10" + "\n" +
                                                  "module_temperature_coefficient=1" + "\n" +
                                                  "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                  + "\n" + "strings_per_inverter=1")
            SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
        # Check correct exception message
        assert 'Input parameters for location seems to be incorrect.' in str(excinfo.value)
        self.tearDown()

    def test_calculate_wrong_timestamp_format(self):
        self.setUp()
        with pytest.raises(Exception) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                  "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                  "\n" + "latitude=1" + "\n" + "longitude=100" + "\n"
                                                  + "altitude=0" + "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=0"
                                                  + "\n" + "module_rated_dc_power=10" + "\n" +
                                                  "module_temperature_coefficient=1" + "\n" +
                                                  "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                  + "\n" + "strings_per_inverter=1")
            SolarModelInstance = SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))
            SolarModel.calculate(SolarModelInstance, "2022-11-23T05:00:0", '', '', '')
        # Check correct exception message
        assert 'timestamp value seems to be in the wrong format, it needs to be in "yyyy-mm-ddThh:mm:ssZ" format' in str(excinfo.value)
        self.tearDown()

    def test_calculate_missing_or_wrong_irradiance(self):
        self.setUp()
        with pytest.raises(Exception) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                      "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                      "\n" + "latitude=1" + "\n" + "longitude=100" + "\n"
                                                      + "altitude=0" + "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=0"
                                                      + "\n" + "module_rated_dc_power=10" + "\n" +
                                                      "module_temperature_coefficient=1" + "\n" +
                                                      "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                      + "\n" + "strings_per_inverter=1")
            SolarModelInstance = SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))

            now = datetime.now(tz=timezone.utc)
            time = self.utcformat(now, 'seconds')
            SolarModel.calculate(SolarModelInstance, "2022-11-23T05:00:01Z", float(10), float(10), 'some_string')
        assert 'Unable to calculate dni and dhi!' in str(excinfo.value)

        with pytest.raises(Exception) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                      "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                      "\n" + "latitude=1" + "\n" + "longitude=100" + "\n"
                                                      + "altitude=0" + "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=0"
                                                      + "\n" + "module_rated_dc_power=10" + "\n" +
                                                      "module_temperature_coefficient=1" + "\n" +
                                                      "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                      + "\n" + "strings_per_inverter=1")
            SolarModelInstance = SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))

            now = datetime.now(tz=timezone.utc)
            time = self.utcformat(now, 'seconds')
            SolarModel.calculate(SolarModelInstance, "2022-11-23T05:00:01Z", float(10), float(10), '')
            # Check correct exception message
        assert 'Unable to calculate dni and dhi!' in str(excinfo.value)
        self.tearDown()

    def test_calculate_wrong_or_missing_air_temperature_and_wind_speed(self):
        self.setUp()
        with pytest.raises(Exception) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                      "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                      "\n" + "latitude=1" + "\n" + "longitude=100" + "\n"
                                                      + "altitude=0" + "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=0"
                                                      + "\n" + "module_rated_dc_power=10" + "\n" +
                                                      "module_temperature_coefficient=1" + "\n" +
                                                      "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                      + "\n" + "strings_per_inverter=1")
            SolarModelInstance = SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))

            SolarModel.calculate(SolarModelInstance, "2022-11-23T05:00:01Z", '', '100', '100')
        assert 'Unable to create dataframe for weather parameters!' in str(excinfo.value)

        with pytest.raises(Exception) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                      "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                      "\n" + "latitude=1" + "\n" + "longitude=100" + "\n"
                                                      + "altitude=0" + "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=0"
                                                      + "\n" + "module_rated_dc_power=10" + "\n" +
                                                      "module_temperature_coefficient=1" + "\n" +
                                                      "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                      + "\n" + "strings_per_inverter=1")
            SolarModelInstance = SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))

            now = datetime.now(tz=timezone.utc)
            time = self.utcformat(now, 'seconds')
            SolarModel.calculate(SolarModelInstance, "2022-11-23T05:00:01Z", '100', '', '100')
        assert 'Unable to create dataframe for weather parameters!' in str(excinfo.value)

        with pytest.raises(Exception) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                      "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                      "\n" + "latitude=1" + "\n" + "longitude=100" + "\n"
                                                      + "altitude=0" + "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=0"
                                                      + "\n" + "module_rated_dc_power=10" + "\n" +
                                                      "module_temperature_coefficient=1" + "\n" +
                                                      "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                      + "\n" + "strings_per_inverter=1")
            SolarModelInstance = SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))

            now = datetime.now(tz=timezone.utc)
            time = self.utcformat(now, 'seconds')
            SolarModel.calculate(SolarModelInstance, "2022-11-23T05:00:01Z", '100', 'some_string', '100')
        assert 'Unable to create dataframe for weather parameters!' in str(excinfo.value)

        with pytest.raises(Exception) as excinfo:
            self._create_temporary_file_with_data(self.temp_path / 'test_solar_model.properties',
                                                      "temp_model=sapm" + "\n" + "temp_model_config=open_rack_glass_glass" +
                                                      "\n" + "latitude=1" + "\n" + "longitude=100" + "\n"
                                                      + "altitude=0" + "\n" + "surface_tilt=0" + "\n" + "surface_azimuth=0"
                                                      + "\n" + "module_rated_dc_power=10" + "\n" +
                                                      "module_temperature_coefficient=1" + "\n" +
                                                      "inverter_rated_dc_power=20" + "\n" + "modules_per_string=1"
                                                      + "\n" + "strings_per_inverter=1")
            SolarModelInstance = SolarModel('ModelChain', '', str(self.temp_path / 'test_solar_model.properties'))

            now = datetime.now(tz=timezone.utc)
            time = self.utcformat(now, 'seconds')
            SolarModel.calculate(SolarModelInstance, "2022-11-23T05:00:01Z", 'some_string', '100', '100')
        assert 'Unable to create dataframe for weather parameters!' in str(excinfo.value)
        self.tearDown()


