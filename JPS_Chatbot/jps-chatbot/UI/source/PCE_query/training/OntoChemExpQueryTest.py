import json

from SPARQLWrapper import SPARQLWrapper, POST, DIGEST, JSON
from openbabel import pybel


def convert_smile_to_inchi(SMILES):
    mol = pybel.readstring("smi", SMILES)
    inchi = mol.write("inchi")
    return str(inchi)


namespace = "opvhopv15"
sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")

sparql.setQuery("""
 
        
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX OntoChemExp: <http://www.theworldavatar.com/ontology/ontochemexp/OntoChemExp.owl#>
        PREFIX OntoSpecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        SELECT ?DonorinChI ?PowerConversionEfficiencyValue
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
              ?DonorCompontentLink OntoSpecies:inChI ?DonorinChI .
              FILTER (?DonorinChI = 'InChI=1S/C30H24N2O2S3Si/c1-33-17-7-12-27-20(14-17)21-15-23(34-2)22(16-28(21)38(27,3)4)26-11-10-25(36-26)19-9-8-18(24-6-5-13-35-24)29-30(19)32-37-31-29/h5-16H,1-4H3'^^xsd:string)
              ?dataGroup OntoChemExp:hasDataPoint ?DataPoint .
              ?DataPoint OntoChemExp:hasDataPointX ?DataPointX .
              ?DataPointX OntoChemExp:refersTo ?Measurement ;
                          OntoChemExp:hasValue ?PowerConversionEfficiencyValue .
              ?Measurement a OntoChemExp:PowerConversionEfficiency .
          }
""")
sparql.setReturnFormat(JSON)
results = sparql.query().convert()

SMILES_LIST = []

for result in results["results"]["bindings"]:
    smiles = (result["DonorinChI"]["value"])
    SMILES_LIST.append(smiles)

print(SMILES_LIST)

# with open('pce_smiles', 'w') as f:
#     f.write(json.dumps(SMILES_LIST))
#     f.close()
#
# for SMILES in SMILES_LIST:
#     inchi = convert_smile_to_inchi(SMILES)
#     print(inchi)