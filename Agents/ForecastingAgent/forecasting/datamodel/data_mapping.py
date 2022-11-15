



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

# Default mapping which uses Prophet and loads just dataIRI without covariates
MAPPING['DEFAULT']=  {
        'frequency': dt.timedelta(hours=1),
        'data_length': 365 * 24,
        'train_again': True,
        'ts_data_type': DOUBLE,
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
        'ts_data_type': MAPPING['DEFAULT']['ts_data_type'],
        'train_again': False,
    }

