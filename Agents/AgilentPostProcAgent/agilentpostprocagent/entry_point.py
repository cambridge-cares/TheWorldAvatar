from agilentpostprocagent.agent import AgilentPostProcAgent, PostProcAgentConfig, default
from pathlib import Path

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)

def create_app():
    config = PostProcAgentConfig(str(Path(__file__).absolute().parent) + '/conf/agent_properties.json')

    app = AgilentPostProcAgent(
        fs_url=config.FILESERVER_URL, fs_user=config.FS_USERNAME, fs_pwd=config.FS_PASSWORD,
        agent_iri=config.ONTOAGENT_SERVICE, time_interval=config.PERIODIC_TIMESCALE,
        derivation_instance_base_url=config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=config.SPARQL_QUERY_ENDPOINT,
        kg_user=config.KG_USERNAME, kg_password=config.KG_PASSWORD,
        logger_name='prod'
    )
    app.add_url_pattern('/', 'root', default, methods=['GET'])

    app.start_monitoring_derivations()
    flask_app = app.app
    return flask_app

if __name__ == '__main__':
    flask_app = create_app()
    flask_app.run()
