# The purpose of this module is to print available HTTP requests (i.e. routes)
# at the application root

from flask import Blueprint

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/epcagent', methods=['GET'])
def default():
    msg  = "The EPC Instantiation Agent offers the following functions via the specified API endpoints:<BR>"
    msg += "(most tasks are handled as background tasks and a task_id is returned upon submitting a HTTP request)<BR>"
    msg += "<BR>"
    msg += "Request to instantiate Local authority district and postcodes based on ONS API (only executed if district not yet instantiated):<BR>"
    msg += "Local authority codes: https://epc.opendatacommunities.org/docs/api/domestic#domestic-local-authority<BR>"
    msg += "&nbsp&nbsp [POST request]   /epcagent/instantiate/postcodes"
    msg += "<BR>"
    msg += "Request to instantiate EPC building data for given single certificate (i.e. single individual lodgement identifier):<BR>"
    msg += "(property data to be newly instantiated if not existing; only updated otherwise)<BR>"
    msg += "&nbsp&nbsp [POST request]   /epcagent/instantiate/certificates/single"
    msg += "<BR>"
    msg += "Request to instantiate latest EPC building data for all instantiated UPRNs in all instantiated postcodes:<BR>"
    msg += "NOTE: Only UPRNs with geospatial representation in provided OntoCityGml namespace are considered<BR>"
    msg += "(property data to be newly instantiated if not existing; only updated otherwise)<BR>"
    msg += "&nbsp&nbsp [POST request]   /epcagent/instantiate/certificates/all"
    msg += "<BR>"
    msg += "<BR>"
    msg += "##### Requests which require prior matching between OntoBuiltEnv and OntoCityGml building instances #####<BR>"
    msg += "(i.e. Building Matching Agent needs to be run beforehand)"
    msg += "<BR>"
    msg += "Request to instantiate relevant OntoCityGml building information according to OntoBuiltEnv<BR>"
    msg += "(i.e. building elevation and footprint uploaded to postgis):<BR>"
    msg += "&nbsp&nbsp [GET request]   /epcagent/add/ocgml_info"
    msg += "<BR>"
    msg += "<BR>"
    msg += "<BR>"
    msg += "##### Check status of submitted request #####<BR>"
    msg += "<BR>"
    msg += "Fetch the result of previously started task with provided <id>:<BR>"
    msg += "&nbsp&nbsp [GET request]   /epcagent/result/<id>"
    msg += "<BR>"
    msg += "<BR>"
    msg += "<BR>"
    msg += "##### Miscellaneous Functionality #####<BR>"
    msg += "This functionality is primarily kept for reference and the Stack Data Uploader should be used instead"
    msg += "<BR>"
    msg += "<BR>"
    msg += "Request to create namespace at specified \"OCGML_ENDPOINT\" and upload previously instantiated and exported quads into it:<BR>"
    msg += "(assumes quad file to be called \"data.nq\" and placed in volume folder \"data\" )<BR>"
    msg += "&nbsp&nbsp [GET request]   /ocgml/initialise"
    return msg
