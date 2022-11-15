### --- Properties for the ForecastingAgent ---###
from forecasting.utils.useful_queries import get_covs_heat_supply

import datetime as dt
# data length is the length of the time series which is loaded as input to the model
# prophet will use this length to fit a model, but TFT will use it just to scale the data and then uses 'input_chunk_length' to predict

TIME_FORMAT = "%Y-%m-%dT%H:%M:%SZ"

MAPPING = {}


# Default mapping which uses Prophet and loads just dataIRI without covariates
MAPPING['DEFAULT']=  {
        'frequency': dt.timedelta(hours=1),
        'data_length': 365 * 24,
        'train_again': True,
    }

MAPPING['TFT_HEAT_SUPPLY'] = {
        'load_covariates_func': get_covs_heat_supply,
        'model': {
            "model_path_ckpt_link":  "https://www.dropbox.com/s/fxt3iztbimvm47s/best.ckpt?dl=1",
            "model_path_pth_link":  "https://www.dropbox.com/s/ntg8lgvh01x09wr/_model.pth.tar?dl=1",
        },
        'frequency': MAPPING['DEFAULT']['frequency'],
        'data_length': MAPPING['DEFAULT']['data_length'],
        'scale_data': True,
    }

