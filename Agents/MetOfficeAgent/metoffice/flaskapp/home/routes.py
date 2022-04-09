from flask import Blueprint

#import agentlogging

# Initialise logger
#logger = agentlogging.get_logger("dev")

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg  = "To see the result of an API call, enter an URL of the form:<BR>"
    msg += "<BR>"
    msg += "Request to instantiate all Met Office stations (GET request):<BR>"
    msg += "&nbsp&nbsp /api/metofficeagent/instantiate/stations"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to instantiate Met Office readings for instantiated stations (GET request):<BR>"
    msg += "&nbsp&nbsp /api/metofficeagent/instantiate/readings"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to add latest time series readings for all instantiated time series (GET request):<BR>"
    msg += "&nbsp&nbsp /api/metofficeagent/update/timeseries"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to update all stations and readings (i.e. instantiate missing stations), "
    msg += "and add latest time series readings for all time series (GET request):<BR>"
    msg += "&nbsp&nbsp /api/metofficeagent/update/all"
    return msg
