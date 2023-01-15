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
    msg  = 'Hi there, The LSOAProbe agent offers the following functions :<BR>'
    msg += "<BR>"
    msg += " ---------------------- Data Download ------------------------------------"
    msg += "<BR>"
    msg += "Request to download xslx file of UK subregional (LSOA) Electricity Consumption/meter data (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/download/electricity"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to download xslx file of UK subregional (LSOA) Gas Consumption/meter/nonmeter data (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/download/gas"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to download xslx file of UK subregional (LSOA) fuel poverty data (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/download/fuelpoverty"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to download nc files of hadUK climate data in 1km grid (GET request):<BR>"
    msg += "NOTE: DON'T forget to registrate an account at CEDA (https://services.ceda.ac.uk/cedasite/register/info/), and specify username and password as per instruction on README file <BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/download/temperature"
    msg += "<BR>"
    msg += "<BR>"
    msg += " ---------------------- Data Instantiation ------------------------------------"
    msg += "<BR>"
    msg += "Request to download and instantiate all Electricity Consumption/meter data to KG (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/instantiate/electricity"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to download and instantiate all UK subregional (LSOA) Gas Consumption/meter/nonmeter data to KG (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/instantiate/gas"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to download and instantiate all UK subregional (LSOA) fuel poverty data to KG (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/instantiate/fuelpoverty"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to download and instantiate all hadUK climate data in 1km grid to KG (GET request):<BR>"
    msg += "NOTE: DON'T forget to registrate an account at CEDA (https://services.ceda.ac.uk/cedasite/register/info/), and specify username and password as per instruction on README file <BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/instantiate/temperature"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to download and instantiate all LSOA geometric shape to KG (GET request):<BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/instantiate/shape"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to download and instantiate all data as mentioned above to KG (GET request):<BR>"
    msg += "NOTE: DON'T forget to registrate an account at CEDA (https://services.ceda.ac.uk/cedasite/register/info/), and specify username and password as per instruction on README file <BR>"
    msg += "&nbsp&nbsp /api/electricityconsumptionagent/instantiate/all"
    msg += "<BR>"
    msg += "<BR>"
    msg += " --------------------------- Data Output ----------------------------------------"
    msg += "<BR>"
    return msg
