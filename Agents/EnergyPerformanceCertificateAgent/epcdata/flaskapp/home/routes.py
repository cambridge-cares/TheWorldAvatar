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
    # msg += "<BR>"
    # msg += "<BR>"
    # msg += "Request to instantiate Met Office readings for instantiated stations (GET request):<BR>"
    # msg += "(only new station readings will be added, already instantiated readings will not be overwritten)<BR>"
    # msg += "&nbsp&nbsp /api/epcagent/instantiate/postcodes"
    return msg
