

from forecasting.datamodel.iris import *


from forecasting.kgutils.javagateway import jpsBaseLibGW

from forecasting.utils.useful_queries import get_covs_heat_supply
from forecasting.datamodel.data_mapping import *

import datetime as dt


# PostgreSQL supported data types: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html
jpsBaseLibView = jpsBaseLibGW.createModuleView()
Instant = jpsBaseLibView.java.time.Instant
TIMECLASS = Instant.now().getClass()

# Create data class for all time series data (i.e. all data as double)
jpsBaseLibView = jpsBaseLibGW.createModuleView()
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
DOUBLE = jpsBaseLibView.java.lang.Double.TYPE
INTEGER = jpsBaseLibView.java.lang.Integer.TYPE
BOOLEAN = jpsBaseLibView.java.lang.Boolean.TYPE


TIME_FORMAT = "%Y-%m-%dT%H:%M:%SZ"

MAPPING = {}
""" KEYS in the MAPPING dict:
Required:
'ts_data_type': Data type from jpsBaseLibView.java.lang
'frequency': The frequency of the time series data
'data_length': The length of the time series data, which is loaded before the forecast_start_data.
    If the data_length is 0, the whole time series data is loaded.
    The data is used to:
        1. Train the model if `train_again` is True or
        2. To scale the data if `scale` is True
        3. Create the forecast, but Neural Methods have fixed input length (same as during training), which is used finally
'fc_model': The forecasting model which is used to create the forecast.
    It is a dict with the following keys:
    Required:
    'name': The name of the forecasting model
    'scale': If True, the data is scaled before the forecast is created
    'train_again': If True, the model is trained again before the forecast is created
    
    Optional:
    If 'model_path_ckpt_link' and 'model_path_pth_link' are give, the model is loaded from the given paths.
    Else Prophet is used. 
    'model_path_ckpt_link': The link to the darts checkpoint file of the model
    'model_path_pth_link': The link to the darts pth file of the model
    

    
"""
# Default mapping which uses Prophet and loads just dataIRI without covariates
MAPPING['DEFAULT'] = {
    'fc_model': {
        'train_again': True,
        'name': 'prophet',
        'scale_data': False,
    },
    'frequency': dt.timedelta(hours=1),
    'data_length': 300,  # 365 * 24 * 2, # 300
    'ts_data_type': DOUBLE,

}

MAPPING['TFT_HEAT_SUPPLY'] = {
    'load_covariates_func': get_covs_heat_supply,
    'fc_model': {
        "model_path_ckpt_link":  "https://www.dropbox.com/s/fxt3iztbimvm47s/best.ckpt?dl=1",
        "model_path_pth_link":  "https://www.dropbox.com/s/ntg8lgvh01x09wr/_model.pth.tar?dl=1",
        'train_again': False,
        'name': 'tft',
        'scale_data': True,
    },
    'frequency': MAPPING['DEFAULT']['frequency'],
    'data_length': MAPPING['DEFAULT']['data_length'],
    'ts_data_type': MAPPING['DEFAULT']['ts_data_type'],
}
