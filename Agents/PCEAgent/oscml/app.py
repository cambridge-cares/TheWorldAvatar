from flask import Flask, jsonify, request
from oscml.jobhandling import JobHandler
from oscml.utils.util import smiles2mol
import os

# Create the Flask app object
app = Flask(__name__)
trained_models = os.path.join('./retrieved_models/trained_models/')

# Show an instructional message at the app root
@app.route('/')
def default():
    msg  = "To see the result of an API call, enter a URL of the form:<BR>"
    msg += "&nbsp&nbsp localhost:5000/api/model/predict?smiles=[SMILES1,SMILES2,..etc]<BR><BR>"
    msg += "&nbsp&nbsp (where SMILESn is a species smiles string identifier)"
    msg += "&nbsp&nbsp example query: "
    msg += "&nbsp&nbsp http://localhost:5000/api/model/predict?smiles=[Cc1nc2c3ccsc3c3sc(-c4ccc(-c5cnc(C6=S=CC=C6)c6nsnc65)s4)cc3c2nc1C]"
    return msg

# Define a route for API requests
@app.route('/api/model/predict', methods=['GET'])
def api():
    # Check arguments (query parameters)
    print(request.args)
    try:
        smiles = request.args['smiles']
    except KeyError:
        print("Error: No 'smiles' parameter provided.")

    for sm in smiles[1:-1].split(','):
        if smiles2mol(sm) is None:
            return jsonify({"status": '500', 'errormsg': 'invalid smiles string: '+sm})

    try:
        model = request.args['model']
    except KeyError:
        # Select a default SVR model
        model = 'svr'

    config = os.path.join(trained_models, model, model + '.json')

    try:
        # Run the model
        args = {'<configFile>': config, '--predict_input': smiles, '--predict': 'true', '--log_sub_dir_prefix': model}
        jobHandler = JobHandler(args)
        jobHandler.runJob()

        # get the results
        result = {'requestedSolarCellDonor': jobHandler.configParams['predict_settings']['predict_input'],
                  'solarCellType': 'organic',
                  'solarCellArchitecture':'bulk heterojunction',
                  'solarCellAcceptor':'fullerene-based',
                  'predictPowerConversionEfficiencyModel': model,
                  'solarCellPowerConversionEfficiency': jobHandler.objective.objParams['predictModel']
                }

        # Return the result in JSON format
        return jsonify({"result": result})
    except Exception as ex:
        print(ex)
        return jsonify({"status": '500'})