################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

from flask import Blueprint #,render_template (this module is for sth looks fancy :) 

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg  = 'Hi there, The LSOAInput agent offers the following functions :<BR>'
    msg += "<BR>"
    msg += "<ul style=\"margin-left: -20px\">"
    msg += "<center> <b> ---------------------- Data Download ------------------------------------</b> </center>"
    msg += "<li> Request to download xslx file of UK subregional (LSOA) Electricity Consumption/meter data (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/download/electricity'> /api/lsoainputagent/download/electricity </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to download xslx file of UK subregional (LSOA) Gas Consumption/meter/nonmeter data (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/download/gas'> /api/lsoainputagent/download/gas </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to download xslx file of UK subregional (LSOA) fuel poverty data (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/download/fuelpoverty'> /api/lsoainputagent/download/fuelpoverty </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to download nc files of hadUK climate data in 1km grid (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/download/temperature'> /api/lsoainputagent/download/temperature </a> </li>"
    msg += "<BR>"
    msg += "<BR>"
    msg += "<center> <b> ---------------------- Data Instantiation ------------------------------------</b> </center>"
    msg += "<li> Request to download and instantiate all Electricity Consumption/meter data to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/electricity'> /api/lsoainputagent/instantiate/electricity </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to download and instantiate all UK subregional (LSOA) Gas Consumption/meter/nonmeter data to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/gas'> /api/lsoainputagent/instantiate/gas </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to download and instantiate all UK subregional (LSOA) fuel poverty data to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/fuelpoverty'> /api/lsoainputagent/instantiate/fuelpoverty </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to download and instantiate all hadUK climate data in 1km grid to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/temperature'>/api/lsoainputagent/instantiate/temperature</a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to download and instantiate all LSOA geometric shape to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/shape'>/api/lsoainputagent/instantiate/shape</a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to download and instantiate all data as mentioned above to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/all'>/api/lsoainputagent/instantiate/all</a> <BR>"
    msg += "<BR>"
    msg += "<ul>"
    return msg
