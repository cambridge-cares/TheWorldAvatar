import flask.scaffold
flask.helpers._endpoint_from_view_func = flask.scaffold._endpoint_from_view_func
from flask import Flask
from flask_restful import Api, Resource

app = Flask(__name__)
api = Api(app)


class TheWorldAvatar(Resource):
    # This is what will happen when a get request is sent to the URL

    def get(self, inchi):
        return {"inchi" : inchi}


# adding the service as a reserouce to the api object
# this is only for non-inputed api
# api2.add_resource(HelloWorld, "/helloworld") # here is endpoint is specified here

api.add_resource(TheWorldAvatar, "/ontospecies/<string:inchi>")
if __name__=="__main__":
    app.run(debug=True)