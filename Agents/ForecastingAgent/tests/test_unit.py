################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 30 Nov 2022                            #
################################################

# This module contains several unit tests for the forecasting agent

import pytest
import pandas as pd

from darts import TimeSeries

from forecastingagent.utils.tools import *
from forecastingagent.datamodel.data_mapping import *
from forecastingagent.datamodel.iris import *
from forecastingagent.forecasting_agent.agent import *


def test_convert_date_to_timestamp():
     """
     Test to convert a date to a timestamp
     """
     # Test if date is converted to timestamp correctly
     assert convert_date_to_timestamp('2019-09-05T09:00:00Z') == 1567674000
     assert convert_date_to_timestamp(1546300800) == 1546300800
     assert convert_date_to_timestamp(pd.Timestamp(isoparse('2019-09-05T09:00:00Z'))) == 1567674000
     with pytest.raises(Exception):
         convert_date_to_timestamp(1.5)


def test_get_ts_lower_upper_bound():
     """
     Test the function to extract the lower and upper bound of the time series data 
     to be retrieved from the database
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
     This function tests if required covariates exist and forecast start date and 
     the forecast horizon are checked properly
     """
     # Test if enough covariates exist
     # create darts Timeseries test covariates
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
