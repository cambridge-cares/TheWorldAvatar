# This script simplifies debugging the agent spun up by the stack manager
#
# Required steps:
# 1) Send request to optimisation trigger agent to create all required derivation
#    markups and create forecasts (expected to fail when reaching the agent)
# 2) Query derivation inputs for debugging/development from Blazegraph GUI
#    prefix deriv: <https://www.theworldavatar.com/kg/ontoderivation/>
#    prefix disp:  <https://www.theworldavatar.com/kg/ontodispersion/>
   
#    select distinct ?input
#    where {     
#         ?deriv deriv:isDerivedFrom ?input
#         {
#             select ?deriv
#             where {
#             ?deriv deriv:isDerivedUsing <https://www.theworldavatar.com/resource/agents/Service__DHEmissionAgent/Service>
# 	        }
#             LIMIT 1
#         }
#     }
#
# 3) Place results from SPARQL query in deriv_inp below
# 4) Start entry_point.py in debug mode
# 5) Run this script in debug mode

from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import ONTODERIVATION_DERIVATION

from emissionagent.datamodel import *
from emissionagent.kgutils.kgclient import KGClient
from emissionagent.utils.env_configs import SPARQL_QUERY_ENDPOINT, \
                                            SPARQL_UPDATE_ENDPOINT


# Define required derivation markup variables
EMISSION_ESTIMATION_AGENT = 'https://www.theworldavatar.com/resource/agents/Service__DHEmissionAgent/Service'
DERIVATION_INSTANCE_BASE_URL = 'https://www.theworldavatar.com/kg/derivation/'

# Initialise sparql and derivation clients
kg_client = KGClient(query_endpoint=SPARQL_QUERY_ENDPOINT, update_endpoint=SPARQL_UPDATE_ENDPOINT)
derivation_client = PyDerivationClient(
    derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
    query_endpoint=SPARQL_QUERY_ENDPOINT, update_endpoint=SPARQL_UPDATE_ENDPOINT)

# Update instantiated agent URL for testing
update = f"""
DELETE {{
    ?ops <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl> ?old_url
}} INSERT {{
    ?ops <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl> "http://localhost:5000/DHEmissionAgent"
}} WHERE {{
    <{EMISSION_ESTIMATION_AGENT}> <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation> ?ops .
    ?ops <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl> ?old_url
}}
"""
kg_client.performUpdate(update)

# Create derivation for new info
deriv_inp = [
    'https://www.theworldavatar.com/kg/pms_dh/ProvidedHeatAmount_c20f6e5d-c58b-4f5c-90ae-1c2ad9e05360',
    'https://www.theworldavatar.com/kg/pms_dh/SimulationTime_b9e81c4f-56c4-4a57-afaf-7060486e1a0d',
    'https://www.theworldavatar.com/kg/pms_dh/StaticPointSource_EfWPlant'
]
derivation_client.createSyncDerivationForNewInfo(EMISSION_ESTIMATION_AGENT, 
                                    deriv_inp, ONTODERIVATION_DERIVATION)

print('done')