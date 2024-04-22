from flask import Blueprint, jsonify

# Blueprint Configuration
gps_bp = Blueprint(
    'gps_bp', __name__
)

# Show an instructional message at the app root
@gps_bp.route('/', methods=['GET'])
def home():
    msg = "<B>Fenland GPS Trajectory Agent offers the following functions via the specified API endpoints:</B><BR>"
    msg += "(Tasks are processed step by step, and you might need to trigger each step separately)<BR><BR><BR>"
    msg += "Process and instantiate GPS trajectory data:<BR>"
    msg += "This service allows for the step-by-step processing and instantiation of GPS data in the target folder collected from Fenland, UK. Below are the steps and corresponding HTTP requests to process and instantiate your GPS data:<BR><BR>"
    msg += "&nbsp;&nbsp;1. <B>[GET request]</B> Process GPS CSV files in the target folder and prepare for instantiation:<BR>"
    msg += "&nbsp;&nbsp;&nbsp;&nbsp;<a href='/process_gps_csv'>/process_gps_csv</a><BR><BR>"
    msg += "&nbsp;&nbsp;2. <B>[GET request]</B> Instantiate processed GPS data into the Knowledge Graph (KG) and PostGIS database:<BR>"
    msg += "&nbsp;&nbsp;&nbsp;&nbsp;<a href='/instantiate_gps_data'>/instantiate_gps_data</a><BR><BR>"
    msg += "Note: The current implementation requires manual placement of .csv files into the target folder. Future updates may include direct upload functionality through this web interface.<BR><BR>"
    msg += "For more detailed instructions or assistance, please refer to the documentation or contact the support team.<BR>"
    return msg
