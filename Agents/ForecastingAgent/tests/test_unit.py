################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
#          Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 25 Jul 2023                            #
################################################

# This module contains several unit tests for the forecasting agent

import pytest
import pandas as pd

import datetime as dt
from darts import TimeSeries
from dateutil.parser import isoparse

# Import modules under test
from forecastingagent.agent.forecasting_tasks import convert_date_to_timestamp, \
                                                     get_ts_lower_upper_bound, \
                                                     check_if_enough_covs_exist

from . import conftest as cf


def test_convert_date_to_timestamp():
    """
    Test if date is converted to timestamp correctly
    """

    # Time stamp in UTC and Singapore time
    assert convert_date_to_timestamp('2019-09-05T09:00:00Z') == 1567674000
    assert convert_date_to_timestamp('2019-09-05T17:00:00+08:00') == 1567674000
    assert convert_date_to_timestamp(1546300800) == 1546300800
    assert convert_date_to_timestamp(pd.Timestamp(isoparse('2019-09-05T09:00:00Z'))) == 1567674000
    assert convert_date_to_timestamp(pd.Timestamp(isoparse('2019-09-05T17:00:00+08:00'))) == 1567674000
    with pytest.raises(Exception):
       convert_date_to_timestamp(1.5)


def test_get_ts_lower_upper_bound():
     """
     Test the function to extract the lower and upper bound of the time series 
     data to be retrieved from the database
     """

     # Test if lower and upper bound are calculated correctly
     start = pd.Timestamp(
            isoparse('2019-09-05T09:00:00Z')).tz_convert('UTC').tz_localize(None)
     cfg = {
          'data_length': 2,
          'horizon': 2,
          'forecast_start_date': start,
          'frequency': dt.timedelta(hours=1),    
     }
     lowerbound_expected = '2019-09-05T07:00:00Z'
     upperbound_expected = '2019-09-05T10:00:00Z'
     assert get_ts_lower_upper_bound(cfg) == (lowerbound_expected, upperbound_expected)


def test_check_if_enough_covs_exist():
     """
     This function tests if required covariates exist and are sufficiently long
     (i.e., cover forecast start date + forecast horizon)
     """

     # Create Darts Timeseries test covariates
     n = 10
     start_date = '2019-09-05T09:00:00Z'
     cfg =  {
          'horizon': n,
          'forecast_start_date': pd.Timestamp(
               isoparse(start_date)).tz_convert('UTC'),
          'frequency': dt.timedelta(hours=1),    
     }
     cov1 = pd.Series(list(range(n)), pd.date_range(start_date, periods=n, freq='H'))
     cov2 = pd.Series(list(range(n)), pd.date_range(start_date, periods=n, freq='H'))
     cov3 = pd.Series(list(range(n)), pd.date_range(start_date, periods=n, freq='H'))
     
     # create df
     df = pd.DataFrame({'cov1': cov1, 'cov2': cov2, 'cov3': cov3})
     covs = TimeSeries.from_dataframe(df)

     cfg['horizon'] = n 
     assert check_if_enough_covs_exist(cfg, covs) == True
     
     # forecast_start_date is not in covs  
     cfg['horizon'] = n + 1
     with pytest.raises(ValueError):
         check_if_enough_covs_exist(cfg, covs)


@pytest.mark.parametrize(
    "derivation_input_set, expected_error_msg",
    [
        (cf.ERROR_INPUTS_1, cf.ERROR_MSG_1),
        (cf.ERROR_INPUTS_2, cf.ERROR_MSG_2),
        (cf.ERROR_INPUTS_3, cf.ERROR_MSG_3),
        (cf.ERROR_INPUTS_4, cf.ERROR_MSG_4),
        (cf.ERROR_INPUTS_5, cf.ERROR_MSG_5)
    ],
)
def test_validate_input_values(
    create_example_agent, derivation_input_set, expected_error_msg
):
    """
    Test whether forecasting agent detects invalid input markups as expected
    """
    
    # Create agent instance without registration in KG
    agent = create_example_agent(register_agent=False)

    with pytest.raises(TypeError) as exc_info:
        # Directly call input validation function and assert exception type
        agent.validate_input_values(inputs=derivation_input_set, derivationIRI='TestDerivation')

    # Check if expected error message is raised
    assert expected_error_msg in str(exc_info.value)
    