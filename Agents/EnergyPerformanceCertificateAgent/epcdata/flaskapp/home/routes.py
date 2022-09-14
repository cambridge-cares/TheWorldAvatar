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
    msg  = "The EPC Instantiation Agent offers the following functions via the specified API endpoints:<BR>"
    msg += "<BR>"
    msg += "Request to initialise knowledge base with TBox and ABox from TWA (only uploads files if ontology is not already initialized in namespace):<BR>"
    msg += "TBox: http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl:<BR>"
    msg += "ABox: http://www.theworldavatar.com/kb/ontobuiltenv/OntoBuiltEnv.owl:<BR>"
    msg += "&nbsp&nbsp [GET request]   /api/epcagent/initialise"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to instantiate Local authority district and postcodes based on ONS API (only executed if district not yet instantiated):<BR>"
    msg += "Local authority codes: https://epc.opendatacommunities.org/docs/api/domestic#domestic-local-authority<BR>"
    msg += "&nbsp&nbsp [POST request]   /api/epcagent/instantiate/postcodes"
    return msg
