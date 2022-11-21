import math

import pandas as pd

import os.path

import pvlib
from configobj import ConfigObj
from pathlib import Path
from flask import jsonify
from pvlib.pvsystem import PVSystem
import datetime

from pvlib.location import Location

from pvlib.modelchain import ModelChain

from pvlib.temperature import TEMPERATURE_MODEL_PARAMETERS

global latitude, longitude


class SolarModel:

    def __init__(self, model_type, latitude_value, longitude_value):
        global latitude, longitude

        # Define location of properties file
        filepath = os.path.abspath(os.path.join(Path(__file__).parent, "resources", "model_parameters.properties"))
        print('The filepath is ' + filepath)

        # Read properties file
        props = ConfigObj(filepath)

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
            latitude = latitude_value

        # Extract longitude
        try:
            longitude = props['longitude']
        except KeyError:
            raise KeyError('Key "longitude" is missing in properties file: ' + filepath)
        if longitude == '':
            longitude = longitude_value

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
            temperature_model_parameters = TEMPERATURE_MODEL_PARAMETERS[temp_model][temp_model_config]
            system = PVSystem(surface_tilt=float(surface_tilt), surface_azimuth=float(surface_azimuth),
                              module_parameters={'pdc0': float(module_rated_dc_power),
                                                 'gamma_pdc': float(module_temp_coefficient)},
                              inverter_parameters={'pdc0': float(inverter_rated_dc_power)},
                              modules_per_string=int(modules_per_string),
                              strings_per_inverter=int(strings_per_inverter),
                              temperature_model_parameters=temperature_model_parameters)

            # load some module and inverter specifications

            location = Location(latitude=float(latitude), longitude=float(longitude), altitude=float(altitude))

            mc = ModelChain(system, location, aoi_model='physical', spectral_model='no_loss')

            self.mc = mc

    def calculate(self, latitude_value, longitude_value):

        global latitude, longitude

        if latitude == '':
            latitude = latitude_value
        if longitude == '':
            longitude = longitude_value

        # derive day-of-year from date
        date = datetime.date.fromisoformat('2017-04-01')

        # derive declination in radian
        declination_angle_radian = pvlib.solarposition.declination_cooper69(int(date.strftime("%j")))

        # derive equation of time in minutes
        equation_of_time = pvlib.solarposition.equation_of_time_pvcdrom(int(date.strftime("%j")))

        # derive hour angle in degrees

        # create empty df
        df = pd.DataFrame(columns=['Timestamp'])

        # append time series to data frame
        df = df.append({'Timestamp': pd.Timestamp('2017-04-01 12:00:00+08')}, ignore_index=True)

        # create time index
        time_index = pd.DatetimeIndex(df.Timestamp)

        hour_angle = pvlib.solarposition.hour_angle(time_index, float(longitude), equation_of_time)

        # derive solar zenith angle (require latitude, hour angle, declination angle)
        solar_zenith_radian = pvlib.solarposition.solar_zenith_analytical(float(latitude), math.radians(hour_angle),
                                                                          declination_angle_radian)

        # derive direct normal irradiance and diffuse horizontal irradiance
        results = pvlib.irradiance.erbs(896, math.degrees(solar_zenith_radian), int(date.strftime("%j")))
        dni = results.get('dni')
        dhi = results.get('dhi')

        weather = pd.DataFrame([[896, float(dhi), float(dni), 50, 5]],
                               columns=['ghi', 'dhi', 'dni', 'temp_air', 'wind_speed'],
                               index=[pd.Timestamp('2017-04-01 12:00:00+08')])
        # index=[pd.Timestamp('20170401 1200', tz='US/Arizona')])
        self.mc.run_model(weather)
        values_string = {"timestamp": self.mc.results.ac.index[0], "AC Power(W)": self.mc.results.ac[0], "DC Power(W)": self.mc.results.dc[0]}

        return values_string
