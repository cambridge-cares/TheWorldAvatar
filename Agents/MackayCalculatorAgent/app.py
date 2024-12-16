# App: main file of the web app
# ===============================================================================
import os
from flask import Flask, send_from_directory, send_file,abort
from flask_restful import Api, Resource, reqparse
from api.PlotDataHandler import PlotDataHandler
from api.ModelUpdateHandler import ModelUpdateHandler
from utils.config import XLSMMODELPATH, ONEPAGERPATH


app = Flask(__name__, static_url_path='', static_folder='frontend/build')

api = Api(app)

#routes to retreive chart values by lever configuration
api.add_resource(PlotDataHandler, '/data')
#routes to connect to KG
api.add_resource(ModelUpdateHandler, '/update')


#For one-pagers
@app.route("/guides/<topic>/<filename>")
def guide(topic, filename):
    return send_from_directory(os.path.join(ONEPAGERPATH,topic),filename)


#This API is currently removed
#@app.route("/download/model")
#def download():
#    return send_from_directory('model',XLSMMODELPATH.replace('./model/'), as_attachment=True)

# redirect to the index page for any error, this is necessary for React one-page style of frontend
@app.errorhandler(404)
def not_found(path):
    return send_from_directory(app.static_folder,'index.html')
