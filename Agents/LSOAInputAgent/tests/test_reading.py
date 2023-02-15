################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

import pytest
import os

from agent.datainstantiation.readings import *



@pytest.mark.parametrize("year, expectedfilepath", \
[
('2020','./downloads/LSOA_domestic_elec_2010-21.xlsx')
]
)
def test_reading_elec_from_web(year, expectedfilepath):
   read_from_web_elec(year)
   assert os.path.isfile(expectedfilepath) 

@pytest.mark.parametrize("year, expectedfilepath", \
[
('2020','./downloads/LSOA_domestic_gas_2010-21.xlsx')
]
)
def test_reading_gas_from_web(year, expectedfilepath):
   read_from_web_gas(year)
   assert os.path.isfile(expectedfilepath) 

@pytest.mark.parametrize("year, expectedfilepath", \
[
('2020','./downloads/sub-regional-fuel-poverty-2022-tables.xlsx')
]
)
def test_reading_fuel_poor_from_web(year, expectedfilepath):
   read_from_web_fuel_poverty(year)
   assert os.path.isfile(expectedfilepath) 

@pytest.mark.parametrize("year, var_name, expectedfilepath", \
[
('2020','tas','./downloads/tas_hadukgrid_uk_1km_mon_202001-202012.nc'),
('2020','tasmin','./downloads/tasmin_hadukgrid_uk_1km_mon_202001-202012.nc'),
('2020','tasmax','./downloads/tasmax_hadukgrid_uk_1km_mon_202001-202012.nc')
]
)
def test_reading_temp_from_web(year, var_name, expectedfilepath):
   read_from_web_temp(year,var_name)
   assert os.path.isfile(expectedfilepath) 

@pytest.mark.parametrize("expectedfilepath", \
[
('./data/shapes_array')
]
)
def test_shape_file_location(expectedfilepath):
   assert os.path.isfile(expectedfilepath) 

@pytest.mark.parametrize('year, expectedMsg', \
[
(2020, 'Provided formate of year is not string')
])
def test_year_format(year, expectedMsg):
    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        read_from_web_elec(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        read_from_web_gas(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        read_from_web_fuel_poverty(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        read_from_web_temp(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

@pytest.mark.parametrize('var_name, expectedMsg', \
[
('Tas', 'Not a valid var_name provided. Please check the spelling/Capitals etc etc...')
])
def test_temperature_var_format(var_name, expectedMsg):
    with pytest.raises(InvalidInput) as excinfo:
       # Check correct exception type
      read_from_web_temp('2020', var_name)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)