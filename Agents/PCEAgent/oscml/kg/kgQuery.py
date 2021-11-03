from oscml.kg.kgGateway import jpsBaseLibGW
import ast

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def queryKG(sparqlEndPoint=None, queryStr=None):
    queryStr = "\n".join(queryStr)

    #queryStr = """
    #  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    #  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    #  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    #  PREFIX OntoChemExp: <http://www.theworldavatar.com/ontology/ontochemexp/OntoChemExp.owl#>
    #  PREFIX OntoSpecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    #
    #  SELECT ?DonorSMILES ?PowerConversionEfficiencyValue
    #  WHERE {
    #  ?exp OntoChemExp:hasCommonProperties ?commonProperties ;
    #       OntoChemExp:hasDataGroup ?dataGroup .
    #  ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity1 .
    #  ?DimQuantity1 a OntoChemExp:JunctionArchitecture .
    #  ?DimQuantity1 OntoChemExp:hasValue ?JunctionArchitectureValue .
    #  FILTER regex(str(?JunctionArchitectureValue), 'bulk heterojunction')
    #
    #  ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity2 .
    #  ?DimQuantity2 a OntoChemExp:Acceptor .
    #  ?DimQuantity2 OntoChemExp:hasComponent ?AcceptorCompontent .
    #  ?AcceptorCompontent OntoChemExp:hasSpeciesLink ?AcceptorCompontentLink .
    #  ?AcceptorCompontentLink OntoChemExp:hasDatPreferredKey ?AcceptorType .
    #  FILTER (?AcceptorType != 'TiO2'^^xsd:string)
    #
    #  ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity3 .
    #  ?DimQuantity3 a OntoChemExp:Donor .
    #  ?DimQuantity3 OntoChemExp:hasComponent ?DonorCompontent .
    #  ?DonorCompontent OntoChemExp:hasSpeciesLink ?DonorCompontentLink .
    #  ?DonorCompontentLink OntoSpecies:SMILES ?DonorSMILES .
    #
    #  ?dataGroup OntoChemExp:hasDataPoint ?DataPoint .
    #  ?DataPoint OntoChemExp:hasDataPointX ?DataPointX .
    #  ?DataPointX OntoChemExp:refersTo ?Measurement ;
    #              OntoChemExp:hasValue ?PowerConversionEfficiencyValue .
    #  ?Measurement a OntoChemExp:PowerConversionEfficiency .
    #}"""
    KGClient = jpsBaseLib_view.RemoteKnowledgeBaseClient(sparqlEndPoint)
    response = KGClient.executeQuery(queryStr)
    response = str(response)
    response = ast.literal_eval(response)

    return response