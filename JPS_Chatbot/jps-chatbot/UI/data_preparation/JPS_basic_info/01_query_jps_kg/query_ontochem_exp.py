import json
import os

from location import TRAINING_FILES_DIR

from SPARQLWrapper import SPARQLWrapper, JSON


def fire_query(query):
    SMILES = []
    namespace = "opvhopv15"
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()

    for result in results["results"]["bindings"]:
        SMILES.append(result['SMILES']['value'])

    return SMILES


SMILES_QUERY_FOR_PCE = '''
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX OntoChemExp: <http://www.theworldavatar.com/ontology/ontochemexp/OntoChemExp.owl#>
PREFIX OntoSpecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT ?SMILES ?PowerConversionEfficiencyValue
WHERE {
      ?exp OntoChemExp:hasCommonProperties ?commonProperties ;
           OntoChemExp:hasDataGroup ?dataGroup .
      ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity1 .
      ?DimQuantity1 a OntoChemExp:JunctionArchitecture .
      ?DimQuantity1 OntoChemExp:hasValue ?JunctionArchitectureValue .
      FILTER regex(str(?JunctionArchitectureValue), 'bulk heterojunction')
      ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity2 .
      ?DimQuantity2 a OntoChemExp:Acceptor .
      ?DimQuantity2 OntoChemExp:hasComponent ?AcceptorCompontent .
      ?AcceptorCompontent OntoChemExp:hasSpeciesLink ?AcceptorCompontentLink .
      ?AcceptorCompontentLink OntoChemExp:hasDatPreferredKey ?AcceptorType .
      FILTER (?AcceptorType != 'TiO2'^^xsd:string)
      ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity3 .
      ?DimQuantity3 a OntoChemExp:Donor .
      ?DimQuantity3 OntoChemExp:hasComponent ?DonorCompontent .
      ?DonorCompontent OntoChemExp:hasSpeciesLink ?DonorCompontentLink .
      ?DonorCompontentLink OntoSpecies:SMILES ?SMILES .
      ?dataGroup OntoChemExp:hasDataPoint ?DataPoint .
      ?DataPoint OntoChemExp:hasDataPointX ?DataPointX .
      ?DataPointX OntoChemExp:refersTo ?Measurement ;
                  OntoChemExp:hasValue ?PowerConversionEfficiencyValue .
      ?Measurement a OntoChemExp:PowerConversionEfficiency .

}

'''

SMILES = fire_query(SMILES_QUERY_FOR_PCE)
print(SMILES)

with open(os.path.join(TRAINING_FILES_DIR, 'PCE_SMILES_LIST'), 'w') as f:
    f.write(json.dumps(SMILES))
    f.close()


