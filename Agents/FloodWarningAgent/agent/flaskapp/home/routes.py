# The purpose of this module is to print available HTTP requests (i.e. routes)
# at the application root

from flask import Blueprint


# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/floodwarnings', methods=['GET'])
def default():
    msg  = "The Flood Warnings Instantiation Agent offers the following functions via the specified API endpoints:<BR>"
    msg += "<BR>"
    msg += "Request to update all flood warnings/alerts and associated flood area (POST request):<BR>"
    msg += "(i.e. instantiate missing ones, update existing ones, and remove outdated ones):<BR>"
    msg += "&nbsp&nbsp /floodwarnings/update/all"
    return msg
