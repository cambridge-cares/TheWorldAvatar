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
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to instantiate EPC building data for given single certificate (i.e. single individual lodgement identifier):<BR>"
    msg += "(Property data to be newly instantiated if not existing; only updated otherwise)<BR>"
    msg += "&nbsp&nbsp [POST request]   /api/epcagent/instantiate/certificates/single"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to instantiate latest EPC building data for all instantiated UPRNs in all instantiated postcodes:<BR>"
    msg += "NOTE: Only UPRNs with geospatial representation in provided OntoCityGml namespace are considered<BR>"
    msg += "(Property data to be newly instantiated if not existing; only updated otherwise)<BR>"
    msg += "&nbsp&nbsp [POST request]   /api/epcagent/instantiate/certificates/all"
    msg += "<BR>"
    msg += "<BR>"
    msg += "<BR>"
    msg += "<BR>"
    msg += "### Requests which require prior matching between OntoBuiltEnv and OntoCityGml building instances ###<BR>"
    msg += "(i.e. Building Matching Agent needs to be run beforehand)"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to instantiate relevant OntoCityGml building information according to OntoBuiltEnv<BR>"
    msg += "(i.e. building elevation and footprint uploaded to postgis):<BR>"
    msg += "&nbsp&nbsp [GET request]   /api/epcagent/add/ocgml_info"
    msg += "<BR>"
    msg += "<BR>"
    msg += "<BR>"
    msg += "<BR>"
    msg += "### Miscellaneous Functionality ###"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to create namespace at specified \"OCGML_ENDPOINT\" and upload previously instantiated and exported quads into it:<BR>"
    msg += "(assumes quad file to be called \"data.nq\" and placed in volume folder \"data\" )<BR>"
    msg += "&nbsp&nbsp [GET request]   /api/ocgml/initialise"
    return msg
