# This script simplifies debugging the district heating optimisation agent as
# spun up by either of the "debug" configurations
#
# Required steps:
# 1) Send request to optimisation trigger agent to create all required derivation
#    markups and create forecasts (expected to fail when reaching optimisation agent)
# 2) Query derivation inputs for debugging/development from Blazegraph GUI
#    prefix deriv: <https://www.theworldavatar.com/kg/ontoderivation/>
#    prefix time:  <http://www.w3.org/2006/time#>   
#    prefix ohn:   <https://www.theworldavatar.com/kg/ontoheatnetwork/>
#    prefix om:    <http://www.ontology-of-units-of-measure.org/resource/om-2/>
   
#    select distinct ?input
#    where {
#     VALUES ?fc_input { ohn:HeatDemand om:Temperature }
#     ?deriv a deriv:DerivationWithTimeSeries ;
#            deriv:isDerivedFrom/a ?fc_input
#     { ?input deriv:belongsTo ?deriv }
#     UNION
#     { ?deriv deriv:isDerivedFrom ?input .
#       ?input a time:Interval }
#    }
#
# 3) Place results from SPARQL query in deriv_inp below
# 4) Start entry_point.py in debug mode
# 5) Run this script in debug mode

from pyderivationagent import PyDerivationClient

from dhoptimisation.datamodel import *
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.utils.env_configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT


# Define required derivation markup variables
DH_OPTIMISATION_AGENT = 'https://www.theworldavatar.com/resource/agents/Service__DHOptimisationAgent/Service'
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
    ?ops <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl> "http://localhost:5000/DHOptimisationAgent"
}} WHERE {{
    <{DH_OPTIMISATION_AGENT}> <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation> ?ops .
    ?ops <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl> ?old_url
}}
"""
kg_client.performUpdate(update)

# Create derivation for new info
deriv_inp = [
    'https://www.theworldavatar.com/kg/forecasting/Forecast_d7033c7e-460d-429c-9483-5b050a192827',
    'https://www.theworldavatar.com/kg/forecasting/Forecast_b9d0bf9f-ba15-47d7-bf3b-2ca53c9d134b',
    'https://www.theworldavatar.com/kg/forecasting/Forecast_e07bea09-fa8f-449b-81c5-ad524cfc5b3d',
    'https://www.theworldavatar.com/kg/forecasting/Forecast_f6731839-04bb-4679-b4df-c5ae8919391e',
    'https://www.theworldavatar.com/kg/forecasting/Forecast_39bf192e-ca6c-4eb5-8214-d9a78e2a9edb',
    'https://www.theworldavatar.com/kg/pms_dh/OptimisationInterval_af2f5ea5-77a4-4a40-b21f-b53391cb7c0c'
]
derivation_client.createSyncDerivationForNewInfo(DH_OPTIMISATION_AGENT, 
                                    deriv_inp, ONTODERIVATION_DERIVATIONWITHTIMESERIES)

print('done')
