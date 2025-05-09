from flask import Flask, g, jsonify, request

from agent.config.stack_configs import retrieve_stack_settings

from agent.stack.ontop_client import OntopClient
from agent.stack.postgis_client import PostGISClient

from twa import JPSGateway
from route import register_route


def create_app():
    app = Flask(__name__, instance_relative_config=True)

    stack_client = JPSGateway('StackClients')
    stack_client.launchGateway()
    # app.extensions['stack_client'] = stack_client
    
    db_conf, ontop_url, blazegraph_url = retrieve_stack_settings(stack_client)
    app.extensions['postgis_client'] = PostGISClient(db_conf)
    app.extensions['ontop_client'] = OntopClient(ontop_url)
        
    register_route(app)
    return app


app = create_app()

if __name__ == '__main__':
    app.run(debug=True)
