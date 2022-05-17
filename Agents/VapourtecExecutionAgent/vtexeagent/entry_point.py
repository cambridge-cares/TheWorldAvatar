from vtexeagent.agent import *

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)


def create_app():
    exe_agent_config = ExeAgentConfig(
        str(Path(__file__).absolute().parent) + '/conf/agent_properties.json')

    app = VapourtecExecutionAgent(exe_agent_config.ONTOAGENT_SERVICE, exe_agent_config.PERIODIC_TIMESCALE,
                                  exe_agent_config.DERIVATION_INSTANCE_BASE_URL, exe_agent_config.SPARQL_QUERY_ENDPOINT, logger_name='prod')
    app.add_url_pattern('/', 'root', default, methods=['GET'])

    app.start_monitoring_derivations()
    flask_app = app.app
    return flask_app


if __name__ == '__main__':
    flask_app = create_app()
    flask_app.run_flask_app()
