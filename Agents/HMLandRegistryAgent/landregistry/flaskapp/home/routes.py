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
    msg  = "The Property Sales Instantiation Agent offers the following functions via the specified API endpoints:<BR>"
    msg += "<BR>"
    msg += "Request to update transaction record(s) for single property/list of instantiated properties:<BR>"
    msg += "(Property IRI(s) to be provided in HTTP request body)<BR>"
    msg += "&nbsp&nbsp [POST request]   /api/landregistry/update"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to update transaction records for all instantiated properties as well as property price index:<BR>"
    msg += "(Instantiates previously not instantiated sales data and updates outdated ones)<BR>"
    msg += "&nbsp&nbsp [POST request]   /api/landregistry/update_all"
    return msg
