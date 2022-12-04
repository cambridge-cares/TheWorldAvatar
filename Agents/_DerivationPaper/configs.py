from pyderivationagent import config_derivation_agent

# This method is used to change the IP address of the agent from the docker internal IP to the host IP
# So that we can create sync derivation for new info (which sends HTTP request to agents) from the markup script
def host_docker_internal_to_localhost(url: str):
    return url.replace('host.docker.internal', 'localhost')

# Endpoints and properties for KG and RDB interaction
# (need to match values specified in docker-compose file)
DB_USER = 'postgres'
DB_PASSWORD = 'postgres'
KG_NAMESPACE = 'mvp'

# Use default 'postgres' namespace for RDB
DB_URL = 'jdbc:postgresql://localhost:9997/postgres'
SPARQL_QUERY_ENDPOINT = f'http://localhost:9998/blazegraph/namespace/{KG_NAMESPACE}/sparql'
SPARQL_UPDATE_ENDPOINT = f'http://localhost:9998/blazegraph/namespace/{KG_NAMESPACE}/sparql'

# Derivation instance base URL
DERIVATION_INSTANCE_BASE_URL = 'https://www.example.com/kg/derivation/'

# Agent env files
AVERAGE_SQUARE_METRE_PRICE_AGENT_ENV = './env_files/agent.avgsqmprice.env'
PROPERTY_VALUE_ESTIMATION_AGENT_ENV = './env_files/agent.propertyvalue.env'
FLOOD_ASSESSMENT_AGENT_ENV = './env_files/agent.floodassessment.env'

# Read environment variables for agents
asmp_agent_config = config_derivation_agent(AVERAGE_SQUARE_METRE_PRICE_AGENT_ENV)
pve_agent_config = config_derivation_agent(PROPERTY_VALUE_ESTIMATION_AGENT_ENV)
fa_agent_config = config_derivation_agent(FLOOD_ASSESSMENT_AGENT_ENV)

# Agent IRI and URL
# 'http://www.theworldavatar.com/resource/agents/Service__KL_AvgSqmPrice/Service'
AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI = asmp_agent_config.ONTOAGENT_SERVICE_IRI
AVERAGE_SQUARE_METRE_PRICE_AGENT_URL = host_docker_internal_to_localhost(asmp_agent_config.ONTOAGENT_OPERATION_HTTP_URL)

# 'http://www.theworldavatar.com/resource/agents/Service__KL_PropertyValueEstimation/Service'
PROPERTY_VALUE_ESTIMATION_AGENT_IRI = pve_agent_config.ONTOAGENT_SERVICE_IRI
PROPERTY_VALUE_ESTIMATION_AGENT_URL = host_docker_internal_to_localhost(pve_agent_config.ONTOAGENT_OPERATION_HTTP_URL)

# 'http://www.theworldavatar.com/resource/agents/Service__KL_FloodAssessment/Service'
FLOOD_ASSESSMENT_AGENT_IRI = fa_agent_config.ONTOAGENT_SERVICE_IRI
FLOOD_ASSESSMENT_AGENT_URL = host_docker_internal_to_localhost(fa_agent_config.ONTOAGENT_OPERATION_HTTP_URL)
