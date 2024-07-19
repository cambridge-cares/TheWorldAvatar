# from flask import Blueprint, jsonify

# # Blueprint Configuration
# gps_bp = Blueprint(
#     'gps_bp', __name__
# )

# # Show an instructional message at the app root
# @gps_bp.route('/', methods=['GET'])
# def home():
#     msg = "<B>Fenland GPS Trajectory Agent offers the following functions via the specified API endpoints:</B><BR>"
#     msg += "(Tasks are processed step by step, and you might need to trigger each step separately)<BR><BR><BR>"
#     msg += "Process and instantiate GPS trajectory data:<BR>"
#     msg += "This service allows for the step-by-step processing and instantiation of GPS data in the target folder collected from Fenland, UK. Below are the steps and corresponding HTTP requests to process and instantiate your GPS data:<BR><BR>"
#     msg += "&nbsp;&nbsp;1. <B>[GET request]</B> Process GPS CSV files in the target folder and prepare for instantiation:<BR>"
#     msg += "&nbsp;&nbsp;&nbsp;&nbsp;<a href='/process_gps_csv'>/process_gps_csv</a><BR><BR>"
#     msg += "&nbsp;&nbsp;2. <B>[GET request]</B> Instantiate processed GPS data into the Knowledge Graph (KG) and PostGIS database:<BR>"
#     msg += "&nbsp;&nbsp;&nbsp;&nbsp;<a href='/instantiate_gps_data'>/instantiate_gps_data</a><BR><BR>"
#     msg += "Note: The current implementation requires manual placement of .csv files into the target folder. Future updates may include direct upload functionality through this web interface.<BR><BR>"
#     msg += "For more detailed instructions or assistance, please refer to the documentation or contact the support team.<BR>"
#     return msg

from flask import Blueprint, render_template_string

# Blueprint Configuration
gps_bp = Blueprint('gps_bp', __name__)

# Show an instructional message at the app root
@gps_bp.route('/', methods=['GET'])
def home():
    msg = """
    <html>
    <head>
        <style>
            body {
                font-family: Arial, sans-serif;
                margin: 40px;
                line-height: 1.6;
            }
            h1 {
                color: #333;
            }
            h2 {
                color: #666;
                font-size: 1.2em;
                margin-top: 1em;
            }
            p {
                font-size: 1em;
                margin-bottom: 1em;
            }
            .note {
                font-size: 0.9em;
                color: #777;
            }
            .button {
                background-color: #4CAF50;
                color: white;
                border: none;
                padding: 10px 20px;
                text-align: center;
                text-decoration: none;
                display: inline-block;
                font-size: 1em;
                margin: 10px 2px;
                cursor: pointer;
                border-radius: 5px;
            }
            .button:hover {
                background-color: #45a049;
            }
        </style>
        <script>
            function sendPostRequest(endpoint, data) {
                var xhr = new XMLHttpRequest();
                xhr.open("POST", endpoint, true);
                xhr.setRequestHeader("Content-Type", "application/json");
                xhr.onreadystatechange = function () {
                    if (xhr.readyState === 4) {
                        if (xhr.status === 200) {
                            alert("Request to " + endpoint + " was successful: " + xhr.responseText);
                        } else {
                            alert("Request to " + endpoint + " failed with status: " + xhr.status + " and response: " + xhr.responseText);
                        }
                    }
                };
                xhr.send(JSON.stringify(data));
            }
            
            function loadAndPreprocess() {
                sendPostRequest('preprocess', {"file_path": "/app/agent/raw_data/gps_target_folder"});
            }

            function processAndInstantiate() {
                sendPostRequest('instantiate', {"file_path": "/app/agent/raw_data/gps_target_folder"});
            }

            function createLayer() {
                var tableName = prompt("Enter the table name for the new layer:");
                if (tableName) {
                    sendPostRequest('layer_generator', {"table_name": tableName});
                }
            }
        </script>
    </head>
    <body>
        <h1>Fenland GPS Trajectory Agent</h1>
        <p><strong>Fenland GPS Trajectory Agent</strong> offers the following functions via the specified API endpoints:</p>
        <p><em>(Tasks are processed step by step, and you might need to trigger each step separately)</em></p>
        
        <h2>Process and Instantiate GPS Trajectory Data</h2>
        <p>This service allows for the step-by-step processing and instantiation of GPS data in the target folder collected from Fenland, UK. Below are the steps and corresponding HTTP requests to process and instantiate your GPS data:</p>
        
        <h2>1. [POST request] Load and preprocess GPS CSV files in the target folder:</h2>
        <button class="button" onclick="loadAndPreprocess()">Load and Preprocess GPS CSV</button>
        
        <h2>2. [POST request] Instantiate processed GPS data into the Knowledge Graph:</h2>
        <button class="button" onclick="processAndInstantiate()">Instantiate GPS Data</button>

        <h2>3. [POST request] Create a new GeoServer layer:</h2>
        <button class="button" onclick="createLayer()">Create GeoServer Layer</button>
        
        <p class="note">Note: The current implementation requires manual placement of .csv files into the target folder. Future updates may include direct upload functionality through this web interface.</p>
        
        <p class="note">For more detailed instructions or assistance, please refer to the documentation or contact the support team.</p>
    </body>
    </html>
    """
    return render_template_string(msg)

