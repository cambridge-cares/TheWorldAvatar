from src.agent import *
# from agent import *

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)

doe_agent_config = DoEAgentConfig(str(Path(__file__).absolute().parent) + '/conf/doeagent_properties.json')

app = DoEAgent(doe_agent_config.ONTOAGENT_SERVICE, doe_agent_config.PERIODIC_TIMESCALE, doe_agent_config.DERIVATION_INSTANCE_BASE_URL, doe_agent_config.SPARQL_QUERY_ENDPOINT, logger_name='prod')
app.add_url_pattern('/', 'root', default, methods=['GET'])

app.start_monitoring_derivations()
flask_app = app.app

if __name__ == '__main__':
    app.run_flask_app()
