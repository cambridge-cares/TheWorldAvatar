from flask import Blueprint, request, jsonify, make_response
from oscml.jobhandling import JobHandler
from oscml.utils.util import smiles2mol
from oscml.kg.get_smiles import get_smiles
import os

KG_END_POINT = "http://kb/ontospecies"
THIS_DIR = os.path.dirname(__file__)
TRAINED_MODELS = os.path.join(THIS_DIR, '..', '..', '..','retrieved_models','trained_models')

pcecalculator_bp = Blueprint(
    'pcecalculator_bp', __name__
)

@pcecalculator_bp.route('/api/model/predict', methods=['GET'])
def api():
    # Check arguments (query parameters)
    print(request.args)
    try:
        spec_iris = request.args['spec_iris']
    except KeyError:
        print("Error: No 'spec_iris' parameter provided.")

    smiles = []
    for siri in spec_iris[1:-1].split(','):
        sm = get_smiles(siri,KG_END_POINT)
        print(sm)
        if smiles2mol(sm) is None:
            return jsonify({"status": '500', 'errormsg': 'invalid smiles string: '+sm})
        smiles.append(sm)
    smiles = '['+','.join(smiles)+']'

    try:
        model = request.args['model']
    except KeyError:
        # Select a default SVR model
        model = 'svr'

    config = os.path.join(TRAINED_MODELS, model, model + '.json')

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