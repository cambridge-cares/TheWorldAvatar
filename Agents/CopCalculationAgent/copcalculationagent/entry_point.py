# Import agent, root web page, and configuration
from pyderivationagent.conf import config_derivation_agent

from copcalculationagent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from copcalculationagent.agent import COPCalculationAgent
from copcalculationagent.agent import default
from copcalculationagent.kg_operations.tsclient import TSClient

def create_app():
    # Depending on the deployment environment, different ways to retrieve/set the 
    # environment variables for the Derivation Agent are required:
    # 1) For deployment as Flask App within Docker container as part of the Stack
    #       a) Define environment variables in `environment` node of the `docker-compose.yml` file
    #       b) Retrieve environment variables using
    #               agent_config = config_derivation_agent()
    #
    # 2) For deployment as Flask App within Docker container, but outside the Stack
    #    (i.e. using 'docker compose up' with docker-compose.yml and agent.env file)
    #       a) Create `agent.env` file (based on `agent.env.example`)
    #       b) Include `env_file` node in `docker-compose.yml` with path to the `agent.env` file
    #       c) Retrieve environment variables using:
    #               agent_config = config_derivation_agent()
    #
    # 3) For deployment as Flask App outside Docker container 
    #       a) Create `agent.env` file (based on `agent.env.example`)
    #       b) Retrieve environment variables using local path to .env file:
    #               agent_config = config_derivation_agent('./agent.env')
    agent_config = config_derivation_agent(env_file='./agent.env.example')

    #run_markup()

    agent = COPCalculationAgent(
        # Settings read from environment variables (.env file, docker-compose)
        register_agent=agent_config.REGISTER_AGENT,
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI, 
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        # Settings read from Stack Clients
        kg_url=QUERY_ENDPOINT,
        kg_update_url=UPDATE_ENDPOINT,      
        # Miscellaneous settings
        logger_name='dev',
        max_thread_monitor_async_derivations=1
    )

    # Update assumptions provided
    try: 
        with open('state.txt', "r") as file:
            has_function_run = file.read().strip() == "True"
    except:
        has_function_run = False
        with open('state.txt', "w") as file:
            file.write("False")  # Initialize the file with "False"
            print("Assumptions has been updated!")

    if not has_function_run:
        agent.sparql_client.update_assumptions()
        with open('state.txt', "w") as file:
            file.write("True")

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    # USE when asyn-mode
    agent.start_all_periodical_job()

    # Expose flask app of agent
    return agent.app

#create_app()
