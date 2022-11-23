

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

MODEL_MAPPING = {}
""" KEYS in the MODEL_MAPPING dict:
Required:
'ts_data_type': Data type from jpsBaseLibView.java.lang
'frequency': The frequency of the time series data
'data_length': The maximum length of the time series data, which is loaded before the forecast_start_data.
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
    
Optinal:
'load_covariates_func': The function which is used to load the covariates. If not provided, no covariates are loaded.
    Be aware, that the returend covariates must have be given for the whole 'horizon'. 
    If the covariates are not long enough, Prophet is used, which does not require covariates
    .
    The function must return parameters:
    'covariates_iris': A list if iris, which are used.
    'covariates': A darts series object, which can be passed into model.predict()
    
    The function will recieve the following parameters:
    'kgClient': The kgClient
    'tsClient': The tsClient
    'lowerbound': The lowerbound of the time series data (can be None)
    'upperbound': The upperbound of the time series data (can be None)

    
"""
# Default mapping which uses Prophet and loads just iri without covariates
MODEL_MAPPING['DEFAULT'] = {
    'fc_model': {
        'train_again': True,
        'name': 'prophet',
        'scale_data': False,
    },
    'frequency': dt.timedelta(hours=1),
    'data_length': 1000,  # 365 * 24 * 2, # 300
    'ts_data_type': DOUBLE,

}

MODEL_MAPPING['TFT_HEAT_SUPPLY'] = {
    'load_covariates_func': get_covs_heat_supply,
    'fc_model': {
        "model_path_ckpt_link":  "https://www.dropbox.com/s/fxt3iztbimvm47s/best.ckpt?dl=1",
        "model_path_pth_link":  "https://www.dropbox.com/s/ntg8lgvh01x09wr/_model.pth.tar?dl=1",
        'train_again': False,
        'name': 'tft',
        'scale_data': True,
    },
    'frequency': MODEL_MAPPING['DEFAULT']['frequency'],
    'data_length': MODEL_MAPPING['DEFAULT']['data_length'],
    'ts_data_type': MODEL_MAPPING['DEFAULT']['ts_data_type'],

}
