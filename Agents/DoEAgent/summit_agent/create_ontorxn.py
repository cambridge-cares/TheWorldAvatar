from .resources.parameter import *
from .resources.doeagent_properties import *
from .kgUtils import *

from summit.utils.dataset import DataSet
from rdflib import Graph, Literal, URIRef
from rdflib.namespace import RDF
import uuid
import os

class ReactionVariation:
    def __init__(self, rxnIRI, rxn_conditions: list, perf_indicators: list, query_endpoint, update_endpoint):
        self.rIRI = URIRef(rxnIRI)
        self.rxnvar = ""
        self.conditions = rxn_conditions
        self.indicators = perf_indicators
        self.g = Graph()
        self.query = query_endpoint
        self.update = update_endpoint
    
    def uploadOntoRxnInstanceToKG(self):
        filePath = f'{str(uuid.uuid4())}.xml'
        self.g.serialize(filePath, format='xml')
        uploadOntology(filePath)
        # os.remove(filePath)
    
    def createOntoRxnInstance(self):
        self.rxnvar = URIRef(NAMESPACE_KB_ONTORXN + getShortName(ONTORXN_REACTIONVARIATION) + '_' + str(uuid.uuid4()))
        
        self.g.add((self.rxnvar, RDF.type, URIRef(ONTORXN_REACTIONVARIATION)))
        self.g.add((self.rxnvar, URIRef(ONTORXN_ISVARIATIONOF), self.rIRI))
        self.g.add((self.rIRI, URIRef(ONTORXN_HASVARIATION), self.rxnvar))

        for con in self.conditions:
            self.addReactionCondition(**con)
        for ind in self.indicators:
            self.addPerformanceIndicator(**ind)
        
        return str(self.rxnvar)

    def addReactionCondition(self, clz, num, id=None, **kwargs):
        """
        rxn: the complete IRI of the reaction experiment (or variation) that the reaction condition is to be added to
        clz: the rdf:type (complete IRI) of the reaction condition to be added
        """
        
        condition_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(clz) + '_' + str(uuid.uuid4()))
        measure_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(OM_MEASURE) + '_' + str(uuid.uuid4()))

        for pred in getObjectRelationship(self.query, self.rIRI, clz):
            self.g.add((self.rxnvar, URIRef(pred), condition_iri))
        
        self.g.add((condition_iri, RDF.type, URIRef(clz)))
        self.g.add((condition_iri, URIRef(OM_HASPHENOMENON), self.rxnvar))
        self.g.add((condition_iri, URIRef(OM_HASVALUE), measure_iri))
        # Only add positionalID if it exists
        if id is not None:
            self.g.add((condition_iri, URIRef(ONTODOE_POSITIONALID), Literal(int(id))))
        # Also add indicatesMultiplicityOf/indicatesUsageOf if it's a OntoRxn:StoichiometryRatio/OntoRxn:ReactionScale
        if (clz == ONTORXN_STOICHIOMETRYRATIO) or (clz == ONTORXN_REACTIONSCALE):
            res = getIndicatesInputChemical(self.query, str(self.rIRI), clz, int(id))
            self.g.add((condition_iri, URIRef(res['indicates']), URIRef(res['inputChem'])))

        self.g.add((measure_iri, RDF.type, URIRef(OM_MEASURE)))
        if id is not None:
            self.g.add((measure_iri, URIRef(OM_HASUNIT), URIRef(getQuantityUnit(self.query, str(self.rIRI), clz, int(id)))))
        else:
            self.g.add((measure_iri, URIRef(OM_HASUNIT), URIRef(getQuantityUnit(self.query, str(self.rIRI), clz, None))))
        self.g.add((measure_iri, URIRef(OM_HASNUMERICALVALUE), Literal(float(num))))

    def addPerformanceIndicator(self, clz, id=None, **kwargs):
        # Create performance indicator instance
        indicator_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(clz) + '_' + str(uuid.uuid4()))
        # Add performance indicator to reaction experiment (or variation)
        self.g.add((self.rxnvar, URIRef(ONTORXN_HASPERFORMANCEINDICATOR), indicator_iri))
        self.g.add((indicator_iri, URIRef(OM_HASPHENOMENON), self.rxnvar))
        # Add rdf:type to created performance indicator instance
        self.g.add((indicator_iri, RDF.type, URIRef(ONTORXN_PERFORMANCEINDICATOR)))
        self.g.add((indicator_iri, RDF.type, URIRef(clz)))
        if id is not None:
            self.g.add((indicator_iri, URIRef(ONTODOE_POSITIONALID), Literal(int(id))))

def uploadNewExpToKG(doe, next_exp: DataSet):
    """The next_exp is expected to be a DataSet
    """
    # print(type(df['ContinuousVariable_1'][0]))
    # rxn_conditions = [{"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#StoichiometryRatio', "id": 2, "num": 3}, \
    # {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#StoichiometryRatio', "id": 2, "num": 0.05}, \
    # {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#ReactionTemperature', "num": 60}, \
    # {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#ResidenceTime', "num": 5}, ]
    # perf_indicators = [{"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#Yield'}, \
    # {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#RunMaterialCost', "id": 7}, \
    # {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#SpaceTimeYield', "id": 8, "num": 0.5}]
    # endpoint = "http://theworldavatar.com/blazegraph/namespace/textontorxn/sparql"

    new_exp_iri_list = []
    
    for i in range(len(next_exp)):
        for var in doe['continuousVariables']:
            var['num'] = next_exp[var['name']][i]
        
        new_exp_ = ReactionVariation(doe['first_exp'], doe['continuousVariables'], doe['systemResponses'], SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT)
        new_exp_iri = new_exp_.createOntoRxnInstance()
        new_exp_.uploadOntoRxnInstanceToKG()
        new_exp_iri_list.append(new_exp_iri)

    return new_exp_iri_list
