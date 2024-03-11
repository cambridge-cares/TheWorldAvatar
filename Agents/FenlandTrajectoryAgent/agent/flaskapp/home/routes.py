from flask import Blueprint, render_template

# Blueprint Configuration
from flask import Blueprint

# Blueprint Configuration
gps_bp = Blueprint(
    'gps_bp', __name__
)

# Show an instructional message at the app root
@gps_bp.route('/', methods=['GET'])
def home():
    # HTML content can be replaced or enhanced by using templates and render_template function.
    # For simplicity, direct HTML content is provided here.
    msg  = 'Welcome to the GPS Data Instantiation Service!<BR>'
    msg += "<BR>"
    msg += "This service allows you to process and instantiate GPS trajectory data into a Knowledge Graph (KG) and PostGIS database. Here’s how you can get started:<BR>"
    msg += "<BR>"
    msg += "<ul style=\"margin-left: -20px\">"
    msg += "<center><b>---------------------- GPS Data Instantiation ------------------------------------</b></center>"
    msg += "<li> Upload and instantiate GPS trajectory data: </li>"
    msg += "&nbsp;&nbsp;You can instantiate your GPS trajectory data by placing the .csv files in the specified target folder. The system automatically processes and instantiates this data into a Knowledge Graph (KG) and PostGIS database for further analysis."
    msg += "<BR><BR>"
    msg += "Note: The current implementation requires manual placement of .csv files into the target folder. Future updates may include direct upload functionality through this web interface."
    msg += "<BR><BR>"
    msg += "For more detailed instructions or assistance, please refer to the documentation or contact the support team."
    msg += "<BR>"
    msg += "<ul>"

    # Using direct HTML for simplicity. For a more complex application, consider using render_template.
    return msg

# Additional routes for specific functionalities can be defined here.
