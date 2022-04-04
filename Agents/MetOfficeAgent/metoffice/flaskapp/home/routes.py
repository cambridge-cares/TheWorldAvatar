from flask import Blueprint

#import agentlogging

# # Initialise logger
# logger = agentlogging.get_logger("dev")

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
    # TODO enter further HTTP request templates
    #msg += "&nbsp&nbsp /api/metofficeagent/calculate?ontocompchem_IRI=oc_IRI&ontospecies_IRI=os_IRI"
    #msg += "&temperature=temperature&pressure=pressure"
    return msg