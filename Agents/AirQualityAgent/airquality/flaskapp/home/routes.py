# The purpose of this module is to print available HTTP requests (i.e. routes)
# at the application root

from flask import Blueprint

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg  = "The airquality agent offers the following functions via the specified API endpoints:<BR>"
    msg += "<BR>"
    msg += "Request to instantiate all UK-AIR Sensor Observation Service stations (GET request):<BR>"
    msg += "(only new stations will be added, already instantiated stations will not be overwritten)<BR>"
    msg += "&nbsp&nbsp /api/airqualityagent/instantiate/stations"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to instantiate UK-AIR Sensor Observation Service readings for instantiated stations (GET request):<BR>"
    msg += "(only new station readings will be added, already instantiated readings will not be overwritten)<BR>"
    msg += "&nbsp&nbsp /api/airqualityagent/instantiate/readings"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to add latest time series readings for all instantiated time series (GET request):<BR>"
    msg += "&nbsp&nbsp /api/airqualityagent/update/timeseries"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to update all stations and associated readings, and add latest data for all time series (GET request):<BR>"
    msg += "(i.e. instantiate missing stations and readings and append latest time series readings):<BR>"
    msg += "&nbsp&nbsp /api/airqualityagent/update/all"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to retrieve data about UK-AIR Sensor Observation Service stations and create respective output files for DTVF (GET request):<BR>"
    msg += "(i.e. request expects all individual query parameter to be provided in a single nested JSON object with key 'query'):<BR>"
    msg += "&nbsp&nbsp /api/airqualityagent/retrieve/all"
    return msg
