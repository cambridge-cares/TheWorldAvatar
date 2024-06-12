from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris
import time
from resultedconsumptioncalculationagent.datamodel.iris import *
from resultedconsumptioncalculationagent.utils.stack_configs import COP_VAR
from tqdm import tqdm


# ---------------------------- Configs ------------------------------ #
agent_config = config_derivation_agent(env_file='./agent.env.example')
agentIRI = agent_config.ONTOAGENT_SERVICE_IRI
agentURL = agent_config.ONTOAGENT_OPERATION_HTTP_URL
QUERY_ENDPOINT = agent_config.SPARQL_QUERY_ENDPOINT
UPDATE_ENDPOINT = QUERY_ENDPOINT

# iris
DERIVATION_INSTANCE_BASE_URL = 'https://www.example.com/kg/derivation/'
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDF_TYPE =  RDF + 'type'

# ----------------------------- Funcs ------------------------------- #
def Synmarkup(
            derivation_client: PyDerivationClient,
            sparql_client: PySparqlClient,
            cop_min_iri, cop_mean_iri, cop_max_iri,
            elec_consumption_iri : str,
            gas_consumption_iri : str,
            consumption_profile_iri : str,
            boiler_efficiency_iri : str,
            proportion_of_heating_iri : str,
            uptake_iri : str,
            agentIRI : str
        ):
        derivation_iri = retrieve_derivation_iri(sparql_client,cop_min_iri, agentIRI)
        if not derivation_iri :
            input_iris = [cop_min_iri, cop_mean_iri, cop_max_iri, elec_consumption_iri, gas_consumption_iri, proportion_of_heating_iri, boiler_efficiency_iri, uptake_iri, consumption_profile_iri]
            #print(input_iris)
            derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                agentIRI=agentIRI,
                agentURL=agentURL,
                inputsIRI=input_iris,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
        
        else:
            print(f'InputIRI: {cop_min_iri} already have derivation IRI: {derivation_iri}, skipped for now')

def retrieve_derivation_iri(
          sparql_client: PySparqlClient,
          input_iri:str,
          agentIRI:str
):
        query = f"""
            SELECT DISTINCT ?s
            WHERE {{
                ?s <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> <{input_iri}>.
                ?s <{pda_iris.ONTODERIVATION_ISDERIVEDUSING}> <{agentIRI}>.
            }}"""
     
        query = ' '.join(query.split())
        response = sparql_client.performQuery(query)
        if len(response) == 0:
            return None
        else:
            syn_derivation_iri = response[0].get('s')
            return syn_derivation_iri

def retrieve_cop_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT DISTINCT ?region ?cop_iri 
        WHERE {{
        ?region <{REGION_HASCOP}> ?cop_iri.
        ?cop_iri  <{RDF_TYPE}> <{REGION_COP}> .
        }}
        ORDER BY ?region ?cop_iri
        """
        res = sparql_client.performQuery(query_string)
        
        if not res:
            raise IndexError('No cop_iri found -- Are you sure you are using the correct namespace?')
        else:
            cop_iri_list = []
            elec_consumption_iri_list = [] 
            gas_consumption_iri_list = []
            for i in range(0, len(res), 3):
                region_code = res[i]["region"].split("/")[-1]
                cop_iri_group = [row["cop_iri"] for row in res[i:i+3]]
                elec_consumption_iri = ONTOGASGRID + "ElectricityConsumptionMeasure_" + region_code
                gas_consumption_iri  = ONTOGASGRID + "GasConsumptionMeasure_" + region_code
                cop_iri_list.append(cop_iri_group)
                elec_consumption_iri_list.append(elec_consumption_iri)
                gas_consumption_iri_list.append(gas_consumption_iri)

            return cop_iri_list, elec_consumption_iri_list, gas_consumption_iri_list

def retrieve_boiler_efficiency_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT DISTINCT ?boiler_efficiency_iri
        WHERE {{
        ?boiler_efficiency_iri <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_BOILER_EFFICIENCY}> .
        }}
        """
        res = sparql_client.performQuery(query_string)
        if not res:
            raise IndexError('No boiler_efficiency_iri found -- Are you sure you are using the correct namespace?')
        else:
            res = res[0]
            boiler_efficiency_iri = res['boiler_efficiency_iri']

            return boiler_efficiency_iri
      
def retrieve_proportion_of_heating_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT ?proportion_of_heating_iri
        WHERE {{
        ?proportion_of_heating_iri <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_PROPORTION_OF_HEATING}> .
        }}
        """
        res = sparql_client.performQuery(query_string)
        if not res:
            raise IndexError('No proportion_of_heating_iri found -- Are you sure you are using the correct namespace?')
        else:
            res = res[0]
            proportion_of_heating_iri = res['proportion_of_heating_iri']

            return proportion_of_heating_iri

def retrieve_uptake_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT ?uptake_iri
        WHERE {{
        ?uptake_iri <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_UPTAKE}> .
        }}
        """
        res = sparql_client.performQuery(query_string)
        if not res:
            raise IndexError('No uptake_iri found -- Are you sure you are using the correct namespace?')
        else:
            res = res[0]
            uptake_iri = res['uptake_iri']

            return uptake_iri
        
def retrieve_consumption_profile_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT ?consumption_profile_iri 
        WHERE {{
         ?country_iri <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> ?consumption_profile_iri .
         ?consumption_profile_iri <{RDF_TYPE}> <{REGION_ENERGYCONSUMPTION_PROFILE}> ;
                                <{OFP_VALIDFROM}> "2020-01-01T12:00:00.000Z"^^<{XSD_DATETIME}> ;
                                <{OFP_VALIDTO}> "2020-12-31T12:00:00.000Z"^^<{XSD_DATETIME}> .
        }}
        """
        res = sparql_client.performQuery(query_string)
        if not res:
            raise IndexError('No consumption_profile_iri found -- Are you sure you are using the correct namespace?')
        else:
            res = res[0]
            consumption_profile_iri = res['consumption_profile_iri']

            return consumption_profile_iri


# ----------------------------- Tasks ------------------------------- #

# Create a PySparqlClient instance
sparql_client = PySparqlClient(
        query_endpoint=QUERY_ENDPOINT,
        update_endpoint=UPDATE_ENDPOINT,
    )

# retrieve cop_iri
cop_iri_list, elec_consumption_iri_list, gas_consumption_iri_list = retrieve_cop_iri(sparql_client)
print(f"A total number of {len(cop_iri_list)*3} will be marked, meaning there is {len(cop_iri_list)} regions will be marked")
consumption_profile_iri = retrieve_consumption_profile_iri(sparql_client)
boiler_efficiency_iri = retrieve_boiler_efficiency_iri(sparql_client)
proportion_of_heating_iri = retrieve_proportion_of_heating_iri(sparql_client)
uptake_iri = retrieve_uptake_iri(sparql_client)
# Create a PyDerivationClient instance
derivation_client = PyDerivationClient(derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
                                        query_endpoint=QUERY_ENDPOINT,
                                        update_endpoint=UPDATE_ENDPOINT)

# Perform Syn markup
for i in tqdm(range(len(cop_iri_list))):
    cop_iris = cop_iri_list[i]
    cop_min_iri = cop_iris[2]
    cop_mean_iri = cop_iris[1]
    cop_max_iri = cop_iris[0]
    elec_consumption_iri = elec_consumption_iri_list[i]
    gas_consumption_iri = gas_consumption_iri_list[i]
    try:
        Synmarkup(
            derivation_client=derivation_client,
            sparql_client = sparql_client,
            cop_min_iri=cop_min_iri,
            cop_mean_iri=cop_mean_iri,
            cop_max_iri=cop_max_iri,
            elec_consumption_iri = elec_consumption_iri,
            gas_consumption_iri = gas_consumption_iri,
            consumption_profile_iri = consumption_profile_iri,
            boiler_efficiency_iri = boiler_efficiency_iri,
            proportion_of_heating_iri = proportion_of_heating_iri,
            uptake_iri = uptake_iri,
            agentIRI = agentIRI
        )
    except:
            derivation_iri = retrieve_derivation_iri(sparql_client, cop_min_iri, agentIRI)
            if not derivation_iri :
                raise KeyError('something wrong, contact Jieyang to fix this')
            else:
                print(f'InputIRI: {cop_min_iri} already have derivation IRI: {derivation_iri}, skipped for now')

# # Perform unified update
# for i in range(len(inputIRI)):
#     time.sleep(1)
#     bday_iri = inputIRI[i]
#     derivation_iri, entities = retrieve_derivation_iri(sparql_client,
#                                                  bday_iri,
#                                                  agentIRI)
#     # Perform unified update
#     derivation_client.unifiedUpdateDerivation(derivation_iri)