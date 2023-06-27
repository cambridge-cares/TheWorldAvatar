# The purpose of this module is to show available HTTP requests (i.e. routes)
# at the application root

from flask import Blueprint


# Instantiates the Blueprint for the homepage
home_bp = Blueprint(
    'home_bp', __name__
)

# Shows an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg  = "The UK Power Generation agent offers the following functions via the specified API endpoints:<BR>"
    msg += "<BR>"
    msg += "Request to download UK Power Generation data from the Balancing Mechanism Reporting Service (BMRS) (GET request):<BR>"
    msg += "&nbsp&nbsp /api/ukpowergenerationagent/download/generators"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to instantiate generatros (GET request):<BR>"
    msg += "(only new generators will be added, already instantiated generators will not be overwritten)<BR>"
    msg += "&nbsp&nbsp /api/ukpowergenerationagent/instantiate/generators"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to retrieve data about generators and create respective output files for the Digital Twin Visulalisation Framework (DTVF) (GET request):<BR>"
    msg += "(i.e. request expects all individual query parameter to be provided in a single nested JSON object with key 'query'):<BR>"
    msg += "&nbsp&nbsp /api/ukpowergenerationagent/retrieve/all"
    return msg
