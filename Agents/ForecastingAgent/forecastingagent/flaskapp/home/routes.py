################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
#          Markus Hofmeister (mh807@cam.ac.uk) #
################################################

# The purpose of this module is to print available HTTP requests (i.e. routes)
# at agent stratup

from flask import Blueprint

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg = '<B>Forecasting agent</B>:<BR><BR>'
    msg += 'The Forecasting Agent can predict instantiated time series and instantiate the forecasted series in the KG.<BR>'
    msg += "It offers the following functions via the specified API endpoints:<BR>"
    msg += "<BR>"
    msg += "Request to forecast the provided dataIRI (POST request):<BR>"
    msg += "(this IRI will also receive the `hasForecastedValue` relationship to the predicted TimeSeries)<BR>"
    msg += '&nbsp&nbsp /forecast'
    msg += '<BR><BR>'
    msg += 'The following parameters are required: <BR><BR>'
    msg += 'iri: the dataIRI of the time series to be forecasted<BR>'
    msg += 'horizon: the number of steps to forecast <BR><BR>'
    msg += 'Further OPTIONAL parameters are supported: <BR>'
    msg += 'Forecast specific parameters: forecast_start_date, data_length, use_model_configuration<BR>'
    msg += 'Connection/stack specific parameters: namespace, database, query_endpoint, update_endpoint, db_url, db_user, db_password <BR><BR>'
    msg += 'For further details please see the <a href="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent/">Forecasting Agent README</a>.'

    return msg

