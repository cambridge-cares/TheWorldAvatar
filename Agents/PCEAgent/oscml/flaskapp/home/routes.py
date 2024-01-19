from flask import Blueprint

# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg  = "To see the result of an API call, enter a URL of the form:<BR>"
    msg += "&nbsp&nbsp localhost:5000/api/model/predict?spec_iris=[IRI1,IRI2,..etc]<BR><BR>"
    msg += "&nbsp&nbsp (where IRIn is a species iri)"
    msg += "&nbsp&nbsp example query: "
    return msg
