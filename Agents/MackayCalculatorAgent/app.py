from flask import Flask, send_from_directory, send_file,abort
from flask_restful import Api, Resource, reqparse
#from flask_cors import CORS #comment this on deployment
from api.PlotDataHandler import PlotDataHandler
from api.ModelUpdateHandler import ModelUpdateHandler
from config import XLSMMODELPATH
app = Flask(__name__, static_url_path='', static_folder='frontend/build')
#CORS(app) #comment this on deployment
api = Api(app)

api.add_resource(PlotDataHandler, '/data')
api.add_resource(ModelUpdateHandler, '/update')

@app.route("/guides/<topic>/<filename>")
def guide(topic, filename):
    app.logger.info('%s', topic)
    app.logger.info('%s', filename)
    return send_from_directory('frontend/guidestest'+'/'+topic,filename)


#This API is currently removed
#@app.route("/download/model")
#def download():
#    return send_from_directory('model',XLSMMODELPATH.replace('./model/'), as_attachment=True)

@app.errorhandler(404)
def not_found(path):
    return send_from_directory(app.static_folder,'index.html')

