from darts.models import TFTModel
from darts import TimeSeries

import pytest
from forecasting.datamodel.data_mapping import *
from forecasting.datamodel.iris import *
from forecasting.utils.tools import *
import pandas as pd
from forecasting.forecasting_agent.agent import *

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
     > Test the function to return the lower and upper bound of the time series data to be retrieved from the
     database
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

def test_load_pretrained_model():
     """
     > Test the function `load_pretrained_model` to load a pretrained model from a checkpoint file or a PyTorch
     model file
     """
     # Test if pretrained model is loaded correctly
     cfg = {
          'model_configuration_name': 'test_model',
          'fc_model': {
               'name': 'TFTModel_test',
               'model_path_ckpt_link': "https://www.dropbox.com/s/fxt3iztbimvm47s/best.ckpt?dl=1",
               'model_path_pth_link': "https://www.dropbox.com/s/ntg8lgvh01x09wr/_model.pth.tar?dl=1",
          },
     }
     model = load_pretrained_model(cfg, TFTModel, forece_download=True)    
     assert model.__class__.__name__ == 'TFTModel'
     assert model.model.input_chunk_length == 168
     assert model.model.output_chunk_length == 24
     
     # use previously downloaded model
     model = load_pretrained_model(cfg, TFTModel, forece_download=False)    
     assert model.__class__.__name__ == 'TFTModel'
     assert model.model.input_chunk_length == 168
     assert model.model.output_chunk_length == 24

def test_check_if_enough_covs_exist():
     """
     > This function test if the covariates exist function for the forecast start date and the forecast horizon works correctly
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
     
     cfg['horizon'] = n + 1
     with pytest.raises(ValueError):
         check_if_enough_covs_exist(cfg, covs)
     # forecast_start_date is not in covs
     
