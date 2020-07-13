from SPARQLWrapper import SPARQLWrapper, JSON, POST
from datetime import datetime
import re
import time
import sys

from test_surrogate_model import run_surrogate_model

def sparqlQueryRead(queryString):
    sparql = SPARQLWrapper("http://localhost:8081/fuseki/test/sparql")
    sparql.setQuery(queryString)
    sparql.setReturnFormat(JSON)

    return sparql.query().convert()

def sparqlQueryWrite(queryString):
    sparql = SPARQLWrapper("http://localhost:8081/fuseki/test/update")
    sparql.setQuery(queryString)
    sparql.setMethod(POST)
    
    return sparql.query()

def getPowerplantInfo(powerplantIRI):
    generationTechnologyMap = {
        'Cogeneration': 'cogeneration',
        'CombinedCycleGasTurbine': 'CCGT',
        'GasEngine': 'Engine',
        'OpenCycleGasTurbine': 'OCGT',
        'SubCriticalThermal': 'subcritical',
        'SuperCriticalThermal': 'supercritical',
        'UltraSuperCriticalThermal': 'ultrasupercritical'
    }

    primaryFuelToFuelUsedMap = {
        'natural_gas': 'gas',
        'oil': 'oil',
        'coal': 'coal',
        'bituminous': 'coal',
        'subbituminous': 'coal',
        'lignite': 'coal',
        'anthracite': 'coal',
        'coal_biomass': 'coal'
    }

    queryString = """
        PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX j6: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
        PREFIX j8: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        PREFIX j5: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
        PREFIX j7: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>

        SELECT ?country ?capacityValue ?year ?primaryFuel ?genTech ?annualGenValue ?genCostValue ?emissionRateValue ?emissionRateValIRI
        WHERE
        {{ 
            GRAPH <{0}>
        {{
            <{0}> j1:hasAddress ?country .

            <{0}> j6:designCapacity ?capacityIRI.
                ?capacityIRI j1:hasValue ?capacity.
                    ?capacity j1:numericalValue ?capacityValue.

            <{0}> j8:hasYearOfBuilt ?yearOfBuilt.
                        ?yearOfBuilt j1:hasValue ?yearValue.
                            ?yearValue j1:numericalValue ?year.

            <{0}> j5:realizes ?generation.
                ?generation j8:consumesPrimaryFuel ?primaryFuel.
                ?generation j8:usesGenerationTechnology ?genTech.
                ?generation j8:hasAnnualGeneration ?annualGenIRI.
                    ?annualGenIRI j1:hasValue ?annualGenValIRI.
                        ?annualGenValIRI j1:numericalValue ?annualGenValue.
                ?generation j7:hasCosts ?genCostIRI.
                    ?genCostIRI j1:hasValue ?genCostValIRI.
                        ?genCostValIRI j1:numericalValue ?genCostValue.
                ?generation j7:hasEmission ?emissionRateIRI.
                    ?emissionRateIRI j1:hasValue ?emissionRateValIRI.
                        ?emissionRateValIRI j1:numericalValue ?emissionRateValue
        }}
        }}
    """.format(powerplantIRI)

    queryResults = sparqlQueryRead(queryString)
    queryResults = queryResults['results']['bindings']

    # get country
    country = re.search(r'/([a-zA-Z_]+)$', str(queryResults[0]['country']['value'])).group(1)

    # get capacity value
    capacityValue = int(queryResults[0]['capacityValue']['value'])

    # get year
    year = int(queryResults[0]['year']['value'])

    # get primary fuel
    primaryFuel = re.search(r'#([a-zA-Z]+)$', str(queryResults[0]['primaryFuel']['value'])).group(1).lower()
    if primaryFuel == "naturalgas":
        primaryFuel = "natural_gas"
    elif primaryFuel == "coalbiomass":
        primaryFuel = "coal_biomass"

    # get generation
    genTechRegexResult = re.search(r'#([a-zA-Z]+)$', str(queryResults[0]['genTech']['value'])).group(1)
    genTech = generationTechnologyMap[genTechRegexResult]

    # get output_MWh (a.k.a. annual generation in knowledge base)
    annualGenValue = float(queryResults[0]['annualGenValue']['value'])

    # fuel_used
    fuelUsed = primaryFuelToFuelUsedMap[primaryFuel]

    # emission_rate
    emissionRate = float(queryResults[0]['emissionRateValue']['value'])

    dict = {}
    dict['country'] = country
    dict['capacity_MW'] = capacityValue
    dict['primary_fuel'] = primaryFuel
    dict['generation_technology'] = genTech
    dict['age'] = datetime.now().year - year
    dict['output_MWh'] = annualGenValue
    dict['fuel_used'] = fuelUsed
    dict['emission_rate'] = emissionRate

    return dict

def return_buildings():

    queryStringWorld = """
        PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

        SELECT ?powerplantIRI
        WHERE
        {
            GRAPH <http://www.theworldavatar.com/kb/powerplants/WorldPowerPlants.owl#WorldPowerPlants>
        {
            <http://www.theworldavatar.com/kb/powerplants/WorldPowerPlants.owl#WorldPowerPlants> system:hasSubsystem ?powerplantIRI .
        }}
    """

    # listDict = []
    result = sparqlQueryRead(queryStringWorld)
    powerplants = result['results']['bindings']
    workingDir = sys.argv[1] + '/'
    
    # print(len(powerplants))
    for powerplant in powerplants:
        start_time = time.time()
        powerplantIRI = powerplant['powerplantIRI']['value']
#         print(powerplantIRI)
        powerplantInfo = getPowerplantInfo(powerplantIRI)
#         print(powerplantInfo)
        updatedPowerplantEmission = run_surrogate_model(powerplantInfo, workingDir)
        updatePowerplantEmission(powerplantIRI, updatedPowerplantEmission)
        # print(getPowerplantInfo(powerplantIRI))
#         print("{} seconds".format(time.time() - start_time))

def updatePowerplantEmission(powerplantIRI, latestEmission):
    # EFF
    queryString = """
            PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX j6: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
            PREFIX j8: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
            PREFIX j5: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
            PREFIX j7: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>

            DELETE {{
                GRAPH <{0}>
                {{
                    ?emissionRateValIRI j1:numericalValue ?emissionRateValue.
            }}
            }}
            INSERT
            {{
                GRAPH <{0}>
                {{
                ?emissionRateValIRI j1:numericalValue {1}.
            }}
            }}
            USING <{0}>
            WHERE
            {{
                <{0}> j5:realizes ?generation.
                    ?generation j7:hasEmission ?emissionRateIRI.
                        ?emissionRateIRI j1:hasValue ?emissionRateValIRI.
                            ?emissionRateValIRI j1:numericalValue ?emissionRateValue.

            }}
        """.format(powerplantIRI, latestEmission)

    sparqlQueryWrite(queryString)


if __name__ == "__main__":

    try:
        start_time = time.time()
        return_buildings()
        print("{} seconds".format(time.time() - start_time))
    except Exception as e:
        print(e)