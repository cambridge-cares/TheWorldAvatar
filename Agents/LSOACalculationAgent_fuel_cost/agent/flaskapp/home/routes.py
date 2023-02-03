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
    with open("./agent/flaskapp/templates/index.html", "r") as file:
        msg = file.read()
    
    return msg