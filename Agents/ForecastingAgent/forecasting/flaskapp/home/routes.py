################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# based on: Markus Hofmeister (mh807@cam.ac.uk)#
################################################
# The purpose of this module is to print available HTTP requests (i.e. routes)

from flask import Blueprint, jsonify

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg = 'Forecasting agent: <BR>'
    msg += 'The forecasting agent can forecast time series and instantiate the new series in the KG. <BR>'
    msg += 'The following parameters are required: <BR><BR>'
    msg += 'iri: (the IRI of the time series to be forecasted), <BR>'
    msg += 'horizon: (the number of steps to forecast) <BR><BR>'
    msg += 'available routes: <BR>'
    msg += '/api/forecastingAgent/forecast <BR><BR>'

    msg += 'Checkout https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent/ for more information.'


    return msg