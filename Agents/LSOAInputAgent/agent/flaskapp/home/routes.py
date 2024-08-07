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
    msg += "<center> <b> ---------------------- Data Instantiation ------------------------------------</b> </center>"
    msg += "<li> Request to instantiate all electricity Consumption/meter data to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/electricity'> /api/lsoainputagent/instantiate/electricity </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to instantiate all UK subregional (LSOA) gas consumption/meter/nonmeter data to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/gas'> /api/lsoainputagent/instantiate/gas </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to instantiate all UK subregional (LSOA) fuel poverty data to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/fuelpoverty'> /api/lsoainputagent/instantiate/fuelpoverty </a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to instantiate all hadUK climate data in 1km grid to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/temperature'>/api/lsoainputagent/instantiate/temperature</a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to instantiate all LSOA geometric shape to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/shape'>/api/lsoainputagent/instantiate/shape</a> <BR>"
    msg += "<BR>"
    msg += "<li> Request to instantiate all data as mentioned above to KG (GET request): </li>"
    msg += "&nbsp&nbsp <a href='/api/lsoainputagent/instantiate/all'>/api/lsoainputagent/instantiate/all</a> <BR>"
    msg += "<BR>"
    msg += "<ul>"
    return msg
