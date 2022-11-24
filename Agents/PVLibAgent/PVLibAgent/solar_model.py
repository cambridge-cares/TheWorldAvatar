import math

import pandas as pd

import os.path

import pvlib
from configobj import ConfigObj
from pathlib import Path
from dateutil.tz import gettz
from pvlib.pvsystem import PVSystem
import datetime

from timezonefinder import TimezoneFinder

from pvlib.location import Location

from pvlib.modelchain import ModelChain

from pvlib.temperature import TEMPERATURE_MODEL_PARAMETERS

from PVLibAgent.data_retrieval.query_data import QueryData
from PVLibAgent.error_handling.exceptions import KGException, SolarModelException

import logging
global latitude, longitude


class SolarModel:

    def __init__(self, model_type, iri, filepath):
        global latitude, longitude

        logging.basicConfig(level=logging.DEBUG)

        # Read properties file
        props = ConfigObj(filepath)
        print(str(filepath))
        # Extract temperature model
        try:
            temp_model = props['temp_model']
        except KeyError:
            raise KeyError('Key "temp_model" is missing in properties file: ' + filepath)
        if temp_model == '':
            raise KeyError('No "temp_model" value has been provided in properties file: ' + filepath)

        # Extract temperature model configuration
        try:
            temp_model_config = props['temp_model_config']
        except KeyError:
            raise KeyError('Key "temp_model_config" is missing in properties file: ' + filepath)
        if temp_model_config == '':
            raise KeyError('No "temp_model_config" value has been provided in properties file: ' + filepath)

        # Extract latitude
        try:
            latitude = props['latitude']
        except KeyError:
            raise KeyError('Key "latitude" is missing in properties file: ' + filepath)
        if latitude == '':
            try:
                latitude = QueryData.query_latitude(iri)
            except Exception as ex:
                logging.error("SPARQL query for latitude not successful.")
                raise KGException("SPARQL query for latitude not successful.") from ex

        # Extract longitude
        try:
            longitude = props['longitude']
        except KeyError:
            raise KeyError('Key "longitude" is missing in properties file: ' + filepath)
        if longitude == '':
            try:
                longitude = QueryData.query_longitude(iri)
            except Exception as ex:
                logging.error("SPARQL query for longitude not successful.")
                raise KGException("SPARQL query for longitude not successful.") from ex

        # Extract altitude
        try:
            altitude = props['altitude']
        except KeyError:
            raise KeyError('Key "altitude" is missing in properties file: ' + filepath)
        if altitude == '':
            raise KeyError('No "altitude" value has been provided in properties file: ' + filepath)

        # Extract surface_tilt
        try:
            surface_tilt = props['surface_tilt']
        except KeyError:
            raise KeyError('Key "surface_tilt" is missing in properties file: ' + filepath)
        if surface_tilt == '':
            raise KeyError('No "surface_tilt" value has been provided in properties file: ' + filepath)

        # Extract surface_azimuth
        try:
            surface_azimuth = props['surface_azimuth']
        except KeyError:
            raise KeyError('Key "surface_azimuth" is missing in properties file: ' + filepath)
        if surface_azimuth == '':
            raise KeyError('No "surface_azimuth" value has been provided in properties file: ' + filepath)

        # Extract module rated DC Power
        try:
            module_rated_dc_power = props['module_rated_dc_power']
        except KeyError:
            raise KeyError('Key "module_rated_dc_power" is missing in properties file: ' + filepath)
        if module_rated_dc_power == '':
            raise KeyError('No "module_rated_dc_power" value has been provided in properties file: ' + filepath)

        # Extract module temperature coefficient
        try:
            module_temp_coefficient = props['module_temperature_coefficient']
        except KeyError:
            raise KeyError('Key "module_temperature_coefficient" is missing in properties file: ' + filepath)
        if module_temp_coefficient == '':
            raise KeyError('No "module_temperature_coefficient" value has been provided in properties file: ' + filepath)

        # Extract inverter rated DC Power
        try:
            inverter_rated_dc_power = props['inverter_rated_dc_power']
        except KeyError:
            raise KeyError('Key "inverter_rated_dc_power" is missing in properties file: ' + filepath)
        if inverter_rated_dc_power == '':
            raise KeyError('No "inverter_rated_dc_power" value has been provided in properties file: ' + filepath)

        # Extract number of modules per string
        try:
            modules_per_string = props['modules_per_string']
        except KeyError:
            raise KeyError('Key "modules_per_string" is missing in properties file: ' + filepath)
        if modules_per_string == '':
            raise KeyError('No "modules_per_string" value has been provided in properties file: ' + filepath)

        # Extract number of strings per inverter
        try:
            strings_per_inverter = props['strings_per_inverter']
        except KeyError:
            raise KeyError('Key "strings_per_inverter" is missing in properties file: ' + filepath)
        if strings_per_inverter == '':
            raise KeyError('No "strings_per_inverter" value has been provided in properties file: ' + filepath)

        if model_type == 'ModelChain':
            try:
                temperature_model_parameters = TEMPERATURE_MODEL_PARAMETERS[temp_model][temp_model_config]
            except Exception as ex:
                raise SolarModelException("Input parameters for constructing the temperature_model_parameters are incorrect") from ex
            try:
                system = PVSystem(surface_tilt=float(surface_tilt), surface_azimuth=float(surface_azimuth),
                                  module_parameters={'pdc0': float(module_rated_dc_power),
                                                 'gamma_pdc': float(module_temp_coefficient)},
                                  inverter_parameters={'pdc0': float(inverter_rated_dc_power)},
                                  modules_per_string=int(modules_per_string),
                                  strings_per_inverter=int(strings_per_inverter),
                                  temperature_model_parameters=temperature_model_parameters)
            except Exception as ex:
                raise SolarModelException("Input parameters for PVSystem seems to be incorrect.") from ex


            # load some module and inverter specifications
            try:
                location = Location(latitude=float(latitude), longitude=float(longitude), altitude=float(altitude))
            except Exception as ex:
                raise SolarModelException("Input parameters for location seems to be incorrect.") from ex

            # create model chain
            mc = ModelChain(system, location, aoi_model='physical', spectral_model='no_loss')

            self.mc = mc

    def calculate(self, timestamp, wind_speed, air_temperature, irradiance):

        global latitude, longitude

        # object creation
        obj = TimezoneFinder()
        timezone = obj.timezone_at(lng=float(longitude), lat=float(latitude))
        print(str(timezone))

        try:
        # timestamp needs to be in this format yyyy-dd-hh'T'hh:mm:ss'Z' e.g. 2017-04-11T10:10:10Z
            dtUTC = datetime.datetime.fromisoformat(timestamp.replace('Z', '+00:00'))

            dtZone = dtUTC.astimezone(gettz(timezone))

            print(dtZone.isoformat(timespec='seconds'))

            # derive day-of-year from date e.g. '2017-04-01'
            date = datetime.date.fromisoformat(str(dtZone)[:10])
        except Exception as ex:
            raise Exception('timestamp value seems to be in the wrong format, it needs to be in "yyyy-mm-ddThh:mm:ssZ" format') from ex

        try:
            # derive declination in radian
            declination_angle_radian = pvlib.solarposition.declination_cooper69(int(date.strftime("%j")))

            # derive equation of time in minutes
            equation_of_time = pvlib.solarposition.equation_of_time_pvcdrom(int(date.strftime("%j")))
        except Exception as ex:
            raise SolarModelException("Unable to calculate declinantion angle and equation of time") from ex

        # create empty df
        df = pd.DataFrame(columns=['Timestamp'])

        # append time series to data frame '2017-04-01 12:00:00+08'
        df = df.append({'Timestamp': pd.Timestamp(str(dtZone), tz=str(timezone))}, ignore_index=True)

        # create time index
        time_index = pd.DatetimeIndex(df.Timestamp)

        # derive hour angle in degrees
        hour_angle = pvlib.solarposition.hour_angle(time_index, float(longitude), equation_of_time)

        # derive solar zenith angle (require latitude, hour angle, declination angle)
        solar_zenith_radian = pvlib.solarposition.solar_zenith_analytical(float(latitude), math.radians(hour_angle),
                                                                          declination_angle_radian)
        try:
            # derive direct normal irradiance and diffuse horizontal irradiance
            results = pvlib.irradiance.erbs(float(irradiance), math.degrees(solar_zenith_radian), int(date.strftime("%j")))
            dni = results.get('dni')
            dhi = results.get('dhi')
        except Exception as ex:
            raise SolarModelException("Unable to calculate dni and dhi!") from ex

        try:
            if air_temperature == '' and wind_speed == '':
                weather = pd.DataFrame(
                    [[float(irradiance), float(dhi), float(dni)]],
                    columns=['ghi', 'dhi', 'dni'],
                    index=[pd.Timestamp(
                        str(dtZone)[:4] + str(dtZone)[5:7] + str(dtZone)[8:10] + ' ' + str(dtZone)[11:13] + str(dtZone)[
                                                                                                        14:16] + str(
                            dtZone)[17:19], tz=str(timezone))])
            else:
                weather = pd.DataFrame([[float(irradiance), float(dhi), float(dni), float(air_temperature), float(wind_speed)]],
                                       columns=['ghi', 'dhi', 'dni', 'temp_air', 'wind_speed'],
                                       index=[pd.Timestamp(str(dtZone)[:4] + str(dtZone)[5:7] + str(dtZone)[8:10] + ' ' + str(dtZone)[11:13] + str(dtZone)[14:16] + str(dtZone)[17:19], tz=str(timezone))])
        except Exception as ex:
            raise SolarModelException("Unable to create dataframe for weather parameters!") from ex

        try:
            self.mc.run_model(weather)
            values_string = {"timestamp": self.mc.results.ac.index[0], "AC Power(W)": self.mc.results.ac[0], "DC Power(W)": self.mc.results.dc[0]}
        except Exception as ex:
            raise SolarModelException("Unable to run Model for calculations!") from ex
        return values_string
