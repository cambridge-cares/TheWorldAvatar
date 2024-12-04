from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris
import time
from inequalityindexagent.datamodel.iris import *
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
            before_utility_cost_iri : str,
            after_utility_cost_iri : str,
            household_iri : str,
            min_fp_iri : str,
            max_fp_iri : str,
            agentIRI : str
        ):

        derivation_iri = retrieve_derivation_iri(sparql_client, before_utility_cost_iri, agentIRI)
        if not derivation_iri :
            input_iris = [before_utility_cost_iri,after_utility_cost_iri, min_fp_iri, max_fp_iri, household_iri]
            #print(input_iris)
            derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                agentIRI=agentIRI,
                agentURL=agentURL,
                inputsIRI=input_iris,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
        
        else:
            print(f'InputIRI: {before_utility_cost_iri} already have derivation IRI: {derivation_iri}, skipped for now')

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

def retrieve_utility_cost_iri_and_household_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT ?region ?before_utility_cost_iri ?after_utility_cost_iri
        WHERE {{
        ?region <{ONTOCAPE_HASUTILITYCOST}> ?before_utility_cost_iri;
                <{ONTOCAPE_HASUTILITYCOST}> ?after_utility_cost_iri.

        ?before_utility_cost_iri <{RDF_TYPE}> <{ONTOCAPE_UTILITYCOST}> ;
                            <{REGION_HASSTATUS}> "Before" .
        ?after_utility_cost_iri <{RDF_TYPE}> <{ONTOCAPE_UTILITYCOST}> ;
                            <{REGION_HASSTATUS}> "After" .
        }}
        """
        res = sparql_client.performQuery(query_string)
        
        if not res:
            raise IndexError('No utility_cost_iri found -- Are you sure you are using the correct namespace?')
        else:
            before_utility_cost_iris_list = []
            after_utility_cost_iris_list = []
            households_iris_list = []
            for i in range(len(res)):
                before_utility_cost_iris_list.append(res[i]['before_utility_cost_iri'])
                after_utility_cost_iris_list.append(res[i]['after_utility_cost_iri'])
                households_iris_list.append(OFP + "Household_" + res[i]["region"].split("/")[-1])
            
            return before_utility_cost_iris_list, after_utility_cost_iris_list, households_iris_list

def retrieve_min_max_fp_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT ?min_fp_iri ?max_fp_iri
        WHERE {{
         ?min_fp_iri <{IS_A}> ?assumption_iri ;
                          <{RDF_TYPE}>  <{REGION_MIN_FP}>.
         ?max_fp_iri <{IS_A}> ?assumption_iri ;
                          <{RDF_TYPE}>  <{REGION_MAX_FP}>.
        }}
        """
        res = sparql_client.performQuery(query_string)
        if not res:
            raise IndexError('No min/max_fp_iri found -- Are you sure you are using the correct namespace?')
        else:
            res = res[0]
            min_fp_iri = res['min_fp_iri']
            max_fp_iri = res['max_fp_iri']

            return min_fp_iri, max_fp_iri

# ----------------------------- Tasks ------------------------------- #

# Create a PySparqlClient instance
sparql_client = PySparqlClient(
        query_endpoint=QUERY_ENDPOINT,
        update_endpoint=UPDATE_ENDPOINT,
    )

# retrieve resulted_consumption_iri
before_utility_cost_iris_list, after_utility_cost_iris_list, households_iris_list = retrieve_utility_cost_iri_and_household_iri(sparql_client)
print(f"A total number of {len(before_utility_cost_iris_list)} will be marked, meaning there is {len(before_utility_cost_iris_list)} regions will be marked")
min_fp_iri, max_fp_iri = retrieve_min_max_fp_iri(sparql_client)

# Create a PyDerivationClient instance
derivation_client = PyDerivationClient(derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
                                        query_endpoint=QUERY_ENDPOINT,
                                        update_endpoint=UPDATE_ENDPOINT)

# Perform Syn markup
for i in tqdm(range(len(before_utility_cost_iris_list))):
    before_utility_cost_iri = before_utility_cost_iris_list[i]
    after_utility_cost_iri = after_utility_cost_iris_list[i]
    household_iri = households_iris_list[i]
    try:
        Synmarkup(
            derivation_client=derivation_client,
            sparql_client = sparql_client,
            before_utility_cost_iri = before_utility_cost_iri,
            after_utility_cost_iri = after_utility_cost_iri,
            household_iri = household_iri,
            min_fp_iri = min_fp_iri,
            max_fp_iri = max_fp_iri,
            agentIRI = agentIRI
        )
    except:
            derivation_iri = retrieve_derivation_iri(sparql_client, before_utility_cost_iri, agentIRI)
            if not derivation_iri :
                raise KeyError('something wrong, contact Jieyang to fix this')
            else:
                print(f'InputIRI: {before_utility_cost_iri} already have derivation IRI: {derivation_iri}, skipped for now')

# # Perform unified update
# for i in range(len(inputIRI)):
#     time.sleep(1)
#     bday_iri = inputIRI[i]
#     derivation_iri, entities = retrieve_derivation_iri(sparql_client,
#                                                  bday_iri,
#                                                  agentIRI)
#     # Perform unified update
#     derivation_client.unifiedUpdateDerivation(derivation_iri)