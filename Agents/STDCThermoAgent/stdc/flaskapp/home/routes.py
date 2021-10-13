from flask import Blueprint

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg  = "To see the result of an API call, enter a URL of the form:<BR>"
    msg += "&nbsp&nbsp /api/thermoagent/calculate?ontocompchem_IRI=oc_IRI&ontospecies_IRI=os_IRI"
    msg += "&temperature=temperature&pressure=pressure"
    return msg