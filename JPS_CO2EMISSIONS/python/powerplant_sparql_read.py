import rdflib
import re
import sys
import json

class PowerplantSPARQLSync:

    def __init__(self, powerplant):
        self.powerplantIRI = powerplant
        self.graph = rdflib.Graph()
        self.graph.parse(self.powerplantIRI)

#         self.powerplantIRI = powerplant
#         self.powerplantName = powerplant[powerplant.rfind('#') + 1:]
#         self.graph = rdflib.Graph()
#         self.graph.parse("C:/TOMCAT/webapps/ROOT/kb/powerplants/{}.owl".format(self.powerplantName))

        self.generationTechnologyMap = {
            'Cogeneration': 'cogeneration',
            'CombinedCycleGasTurbine': 'CCGT',
            'GasEngine': 'Engine',
            'OpenCycleGasTurbine': 'OCGT',
            'SubCriticalThermal': 'subcritical',
            'SuperCriticalThermal': 'supercritical',
            'UltraSuperCriticalThermal': 'ultrasupercritical'
        }

        self.primaryFuelToFuelUsedMap = {
            'natural_gas': 'gas',
            'oil': 'oil',
            'coal': 'coal',
            'bituminous': 'coal',
            'subbituminous': 'coal',
            'lignite': 'coal',
            'anthracite': 'coal',
            'coal_biomass': 'coal'
        }

    def __del__(self):
        pass

    def getPowerplantInfo(self):
        queryString = """
            PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX j6: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
            PREFIX j8: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
            PREFIX j5: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
            PREFIX j7: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>

            SELECT ?country ?capacityValue ?ageValue ?primaryFuel ?genTech ?annualGenValue ?genCostValue ?emissionRateValue
            WHERE
            {{
                <{0}> j1:hasAddress ?country .

                <{0}> j6:designCapacity ?capacityIRI.
                    ?capacityIRI j1:hasValue ?capacity.
                        ?capacity j1:numericalValue ?capacityValue.

                <{0}> j8:hasAge ?ageIRI.
                    ?ageIRI j1:hasValue ?age.
                        ?age j1:numericalValue ?ageValue.

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
        """.format(self.powerplantIRI)

        queryResults = self.graph.query(queryString).bindings

        # get country
        country = re.search(r'/([a-zA-Z_]+)$', str(queryResults[0]['country'])).group(1)

        # get capacity value
        capacityValue = int(queryResults[0]['capacityValue'].toPython())

        # get age
        age = int(queryResults[0]['ageValue'].toPython())

        # get primary fuel
        primaryFuel = re.search(r'#([a-zA-Z]+)$', str(queryResults[0]['primaryFuel'])).group(1).lower()
        if primaryFuel == "naturalgas":
            primaryFuel = "natural_gas"
        elif primaryFuel == "coalbiomass":
            primaryFuel = "coal_biomass"

        # get generation
        genTechRegexResult = re.search(r'#([a-zA-Z]+)$', str(queryResults[0]['genTech'])).group(1)
        genTech = self.generationTechnologyMap[genTechRegexResult]

        # get output_MWh (a.k.a. annual generation in knowledge base)
        annualGenValue = float(queryResults[0]['annualGenValue'].toPython())

        # fuel_used
        fuelUsed = self.primaryFuelToFuelUsedMap[primaryFuel]
        
        # emission_rate
        emissionRate = float(queryResults[0]['emissionRateValue'].toPython())

        dict = {}
        dict['country'] = country
        dict['capacity_MW'] = capacityValue
        dict['primary_fuel'] = primaryFuel
        dict['generation_technology'] = genTech
        dict['age'] = age
        dict['output_MWh'] = annualGenValue
        dict['fuel_used'] = fuelUsed
        dict['emission_rate'] = emissionRate

        return dict
    
if __name__ == "__main__":
    plantIRI = sys.argv[1]
    pSPARQL = PowerplantSPARQLSync(plantIRI)
    powerplantInfo = pSPARQL.getPowerplantInfo()
    print(json.dumps(powerplantInfo))