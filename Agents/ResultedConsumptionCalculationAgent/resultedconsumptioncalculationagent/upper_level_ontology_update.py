import uuid
from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris
import time
import pandas as pd
from tqdm import tqdm
import numpy as np

from resultedconsumptioncalculationagent.datamodel.iris import *

# ---------------------------- Configs ------------------------------ #
agent_config = config_derivation_agent(env_file='./agent.env.example')
agentIRI = agent_config.ONTOAGENT_SERVICE_IRI
agentURL = agent_config.ONTOAGENT_OPERATION_HTTP_URL
QUERY_ENDPOINT = agent_config.SPARQL_QUERY_ENDPOINT
UPDATE_ENDPOINT = QUERY_ENDPOINT
# ----------------------------- Funcs ------------------------------- #
def check_country(sparql_client):
    # check if there is country exist
    query_string = f'''
    SELECT ?country
    {{
    ?country <{RDF_TYPE}> <{ONTOCAPE_COUNTRY}>.
    }}
    '''
    res = sparql_client.performQuery(query_string)

    if not res:
        country_iri = ONTOCAPE + "Country_" + str(uuid.uuid4())
        print(f'No existed country iri, created {country_iri}')
        update_country(sparql_client, country_iri)
    else: 
        res = res[0]
        country_iri = str(res["country"])
        print(f'country iri: {country_iri} will be used')
    
    return country_iri

def update_country(sparql_client, country_iri):
     query_string = f'''
     INSERT DATA {{
     <{country_iri}> <{RDF_TYPE}> <{ONTOCAPE_COUNTRY}>.
     }}
     '''
     sparql_client.performUpdate(query_string)

def region_within_country_update_template(region, country_iri):
     
     triple = f'''
     <{region}> <{REGION_ISWITHIN}> <{country_iri}>.
     '''

     return triple

def ontop_data_backup_template(region):
    region_code = region.split("/")[-1]
    elec_consumption_iri = ONTOGASGRID + "ElectricityConsumptionMeasure_" + region_code
    gas_consumption_iri  = ONTOGASGRID + "GasConsumptionMeasure_" + region_code
    
    triple = f"""
     <{elec_consumption_iri}> <{RDF_TYPE}> <{OM_MEASURE}>.
     <{gas_consumption_iri}> <{RDF_TYPE}> <{OM_MEASURE}>.
     """

    return triple

def initialize_assumptions(sparql_client):

    def get_assumption_iri(sparql_client, country_iri):
        query_string = f"""
        SELECT ?assumption_iri
        WHERE {{
        ?assumption_iri <{REGION_APPLICABLETO}> <{country_iri}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            assumption_iri = REGION + "Assumption_" + str(uuid.uuid4())
            print(f'No existed assumption_iri, created {assumption_iri}')
        else: 
            res = res[0]
            assumption_iri = str(res["assumption_iri"])
            print(f'assumption_iri: {assumption_iri} will be used')
        
        return assumption_iri
    
    def get_heatpumpefficiency_iri(sparql_client, assumption_iri):
        query_string = f"""
        SELECT ?heatpumpefficiency_iri
        WHERE {{
        ?heatpumpefficiency_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_HEATPUMP_EFFICIENCY}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            heatpumpefficiency_iri = REGION + "HeatPumpEfficiency_" + str(uuid.uuid4())
            print(f'No existed heatpumpefficiency_iri, created {heatpumpefficiency_iri}')
        else: 
            res = res[0]
            heatpumpefficiency_iri = str(res["heatpumpefficiency_iri"])
            print(f'heatpumpefficiency_iri: {heatpumpefficiency_iri} will be used')
        
        return heatpumpefficiency_iri
    
    def get_hotsidetemperature_iri(sparql_client, assumption_iri):
        query_string = f"""
        SELECT ?hotsidetemperature_iri
        WHERE {{
        ?hotsidetemperature_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_HOTSIDE_TEMPERATURE}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            hotsidetemperature_iri = REGION + "HotSideTemperature_" + str(uuid.uuid4())
            print(f'No existed hotsidetemperature_iri, created {hotsidetemperature_iri}')
        else: 
            res = res[0]
            hotsidetemperature_iri = str(res["hotsidetemperature_iri"])
            print(f'hotsidetemperature_iri: {hotsidetemperature_iri} will be used')
        
        return hotsidetemperature_iri
    
    def get_boiler_efficiency_iri(sparql_client, assumption_iri):
        query_string = f"""
        SELECT ?boiler_efficiency_iri
        WHERE {{
        ?boiler_efficiency_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_BOILER_EFFICIENCY}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            boiler_efficiency_iri = REGION + "BoilerEfficiency_" + str(uuid.uuid4())
            print(f'No existed boiler_efficiency_iri, created {boiler_efficiency_iri}')
        else: 
            res = res[0]
            boiler_efficiency_iri = str(res["boiler_efficiency_iri"])
            print(f'boiler_efficiency_iri: {boiler_efficiency_iri} will be used')
        
        return boiler_efficiency_iri
    
    def get_proportion_of_heating_iri(sparql_client, assumption_iri):
        query_string = f"""
        SELECT ?proportion_of_heating_iri
        WHERE {{
        ?proportion_of_heating_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_PROPORTION_OF_HEATING}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            proportion_of_heating_iri = REGION + "ProportionofHeating_" + str(uuid.uuid4())
            print(f'No existed proportion_of_heating_iri, created {proportion_of_heating_iri}')
        else: 
            res = res[0]
            proportion_of_heating_iri = str(res["proportion_of_heating_iri"])
            print(f'proportion_of_heating_iri: {proportion_of_heating_iri} will be used')
        
        return proportion_of_heating_iri
    
    def get_uptake_iri(sparql_client, assumption_iri):
        query_string = f"""
        SELECT ?uptake_iri
        WHERE {{
        ?uptake_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_UPTAKE}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            uptake_iri = REGION + "HeatPumpUptake_" + str(uuid.uuid4())
            print(f'No existed uptake_iri, created {uptake_iri}')
        else: 
            res = res[0]
            uptake_iri = str(res["uptake_iri"])
            print(f'uptake_iri: {uptake_iri} will be used')
        
        return uptake_iri

    country_iri = check_country(sparql_client)
    assumption_iri = get_assumption_iri(sparql_client,country_iri)
    # Assumptions for COP agent
    heatpumpefficiency_iri = get_heatpumpefficiency_iri(sparql_client,assumption_iri)
    hotsidetemperature_iri = get_hotsidetemperature_iri(sparql_client,assumption_iri)
    
    # Assumptions for Resulted Consumption agent
    boiler_efficiency_iri = get_boiler_efficiency_iri(sparql_client,assumption_iri)
    proportion_of_heating_iri = get_proportion_of_heating_iri(sparql_client,assumption_iri)
    uptake_iri = get_uptake_iri(sparql_client,assumption_iri)

    query_string = f"""
    INSERT DATA {{
    <{assumption_iri}> <{REGION_APPLICABLETO}> <{country_iri}> .
    <{heatpumpefficiency_iri}> <{IS_A}> <{assumption_iri}> ;
                <{RDF_TYPE}> <{REGION_HEATPUMP_EFFICIENCY}> .
    <{hotsidetemperature_iri}> <{IS_A}> <{assumption_iri}> ;
                <{RDF_TYPE}> <{REGION_HOTSIDE_TEMPERATURE}> .
    <{boiler_efficiency_iri}> <{IS_A}> <{assumption_iri}> ;
                <{RDF_TYPE}> <{REGION_BOILER_EFFICIENCY}> .
    <{proportion_of_heating_iri}> <{IS_A}> <{assumption_iri}> ;
                <{RDF_TYPE}> <{REGION_PROPORTION_OF_HEATING}> .
    <{uptake_iri}> <{IS_A}> <{assumption_iri}> ;
                <{RDF_TYPE}> <{REGION_UPTAKE}> .
    }}
    """
    sparql_client.performUpdate(query_string)

def update_regions_within_country(sparql_client):
    
    # A source file 'LSOA_codes_IRIs.csv' file is needed to get all the IRI for regions
    data = pd.read_csv('LSOA_codes_IRIs.csv')

    # Access the data in the DataFrame
    LSOA_codes = data['LSOA code'].tolist()

    country_iri = check_country(sparql_client)

    update_country(sparql_client, country_iri)
    # Split the queries into Batches
    # Perform SPARQL update query in chunks to avoid heap size/memory issues
    total = len(LSOA_codes)
    n_compile = total / 10
    remainder = total % 10
    n_compile = int(n_compile)
    len_query = np.zeros(n_compile + 2)
    if remainder == 0:
        len_query = np.zeros(n_compile + 1)

    for i in range(1, len(len_query) - 1):
            len_query[i] = len_query[i - 1] + 10
            len_query[-1] = len_query[-2] + remainder

    for g in tqdm(range(len(len_query) - 1)):

        i = int(len_query[g])
        region = LSOA_codes[i]
        # Initialise update query
        query = f"INSERT DATA" + "{"
        region = LSOA_codes[i]
        triple = region_within_country_update_template(region, country_iri)
        middle_num = int(len_query[g + 1] - len_query[g]) - 2
        for j in range(middle_num):
            region = LSOA_codes[i + j + 1]
            triple += region_within_country_update_template(region, country_iri)

        region = LSOA_codes[int(len_query[g + 1]) - 1]
        triple += region_within_country_update_template(region, country_iri)
        query +=triple + "}"
        sparql_client.performUpdate(query)

def initialize_indecies(sparql_client):

    def get_consumption_profile_iri(sparql_client, country_iri):
        query_string = f"""
        SELECT ?consumption_profile_iri ?elec_profile_iri ?gas_profile_iri
        WHERE {{
         <{country_iri}> <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> ?consumption_profile_iri .
         ?consumption_profile_iri <{RDF_TYPE}> <{REGION_ENERGYCONSUMPTION_PROFILE}> ;
                                <{OFP_VALIDFROM}> "2020-01-01 12:00:00"^^<{XSD_DATETIME}> ;
                                <{OFP_VALIDTO}> "2020-12-31 12:00:00"^^<{XSD_DATETIME}> .
         ?elec_profile_iri <{IS_A}> ?consumption_profile_iri;
                          <{RDF_TYPE}>  <{REGION_ELECTRICITYCONSUMPTION_PROFILE}>.
         ?gas_profile_iri <{IS_A}> ?consumption_profile_iri;
                          <{RDF_TYPE}>  <{REGION_GASCONSUMPTION_PROFILE}>.
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            consumption_profile_iri = REGION + "EnergyConsumptionProfile_" + str(uuid.uuid4())
            print(f'No existed consumption_profile_iri, created {consumption_profile_iri}')
            elec_profile_iri = REGION + "ElectricityConsumptionProfile_" + str(uuid.uuid4())
            print(f'No existed elec_profile_iri, created {elec_profile_iri}')
            gas_profile_iri = REGION + "GasConsumptionProfile_" + str(uuid.uuid4())
            print(f'No existed gas_profile_iri, created {gas_profile_iri}')
        else: 
            res = res[0]
            if res["consumption_profile_iri"] == "" or res["consumption_profile_iri"] == None:
                consumption_profile_iri = REGION + "EnergyConsumptionProfile_" + str(uuid.uuid4())
                print(f'No existed consumption_profile_iri, created {consumption_profile_iri}')
            else:
                consumption_profile_iri = str(res["consumption_profile_iri"])
                print(f'consumption_profile_iri: {consumption_profile_iri} will be used')

            if res["elec_profile_iri"] == "" or res["elec_profile_iri"] == None:
                elec_profile_iri = REGION + "ElectricityConsumptionProfile_" + str(uuid.uuid4())
                print(f'No existed elec_profile_iri, created {elec_profile_iri}') 
            else:
                elec_profile_iri = str(res["elec_profile_iri"])
                print(f'elec_profile_iri: {elec_profile_iri} will be used')

            if res["gas_profile_iri"] == "" or res["gas_profile_iri"] == None:
                gas_profile_iri = REGION + "GasConsumptionProfile_" + str(uuid.uuid4())
                print(f'No existed gas_profile_iri, created {gas_profile_iri}')
            else:
                gas_profile_iri = str(res["gas_profile_iri"])
                print(f'gas_profile_iri: {gas_profile_iri} will be used')
        
        return consumption_profile_iri, elec_profile_iri, gas_profile_iri
    
    country_iri = check_country(sparql_client)
    # Initialize consumption profiles
    consumption_profile_iri, elec_profile_iri, gas_profile_iri = get_consumption_profile_iri(sparql_client, country_iri)
    
    query_string = f"""
    INSERT DATA {{
        <{country_iri}> <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> <{consumption_profile_iri}> .
        <{consumption_profile_iri}> <{RDF_TYPE}> <{REGION_ENERGYCONSUMPTION_PROFILE}> ;
                            <{OFP_VALIDFROM}> "2020-01-01 12:00:00"^^<{XSD_DATETIME}> ;
                            <{OFP_VALIDTO}> "2020-12-31 12:00:00"^^<{XSD_DATETIME}> .
        <{elec_profile_iri}> <{IS_A}> <{consumption_profile_iri}>;
                        <{RDF_TYPE}>  <{REGION_ELECTRICITYCONSUMPTION_PROFILE}> .

        <{gas_profile_iri}> <{IS_A}> <{consumption_profile_iri}>;
                        <{RDF_TYPE}>  <{REGION_GASCONSUMPTION_PROFILE}> .

    }}
    """
    res = sparql_client.performUpdate(query_string)

def ontop_data_backup(sparql_client: PySparqlClient):
    
    # A source file 'LSOA_codes_IRIs.csv' file is needed to get all the IRI for regions
    data = pd.read_csv('LSOA_codes_IRIs.csv')

    # Access the data in the DataFrame
    LSOA_codes = data['LSOA code'].tolist()
    

    # Split the queries into Batches
    # Perform SPARQL update query in chunks to avoid heap size/memory issues
    total = len(LSOA_codes)
    n_compile = total / 10
    remainder = total % 10
    n_compile = int(n_compile)
    len_query = np.zeros(n_compile + 2)
    if remainder == 0:
        len_query = np.zeros(n_compile + 1)

    for i in range(1, len(len_query) - 1):
            len_query[i] = len_query[i - 1] + 10
            len_query[-1] = len_query[-2] + remainder

    for g in tqdm(range(len(len_query) - 1)):

        i = int(len_query[g])
        region = LSOA_codes[i]
        # Initialise update query
        query = f"INSERT DATA" + "{"
        region = LSOA_codes[i]
        triple = ontop_data_backup_template(region)
        middle_num = int(len_query[g + 1] - len_query[g]) - 2
        for j in range(middle_num):
            region = LSOA_codes[i + j + 1]
            triple += ontop_data_backup_template(region)

        region = LSOA_codes[int(len_query[g + 1]) - 1]
        triple += ontop_data_backup_template(region)
        query +=triple + "}"
        sparql_client.performUpdate(query)
# ----------------------------- Tasks ------------------------------- #

# Create a PySparqlClient instance
sparql_client = PySparqlClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)

initialize_assumptions(sparql_client)

initialize_indecies(sparql_client)

ontop_data_backup(sparql_client)

update_regions_within_country(sparql_client)


