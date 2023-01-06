################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

from flask import Blueprint

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg  = 'Hi there, before starting the agent please check if the latest excel file have been placed into ./Data/LSOA_domestic_elec_2010-20.xlsx.\
            If file can not be found, maybe check if the name of the excel matching the file name LSOA_domestic_elec_2010-20.xlsx <BR>'
    msg += "<BR>"
    msg += 'If everything is fine, we shall start now :) <BR>'
    msg += "<BR>"
    msg += "The ElectricityConsumption agent offers the following functions :<BR>"
    msg += "<BR>"
    msg += "Request to instantiate all Electricity Consumption to KG (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/instantiate/readings"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to add latest time series readings for all instantiated time series (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/update/timeseries"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to update all Electricity Consumption readings, and add latest data for all time series (GET request):<BR>"
    msg += "(i.e. instantiate readings and append latest time series readings):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/update/all"
    msg += "<BR>"
    msg += "<BR>"
    return msg