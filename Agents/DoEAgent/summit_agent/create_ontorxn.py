from resources.parameter import *
from resources.doeagent_properties import *
from kgUtils import *

from summit.utils.dataset import DataSet
from rdflib import Graph, Literal, URIRef
from rdflib.namespace import RDF
import uuid
import os

class ReactionVariation:
    """
        This class corresponds to the class OntoRxn:ReactionVariation as defined in the OntoRxn ontology.
        It should be used to create instances of OntoRxn:ReactionVariation and upload them to the knowledge graph.
    """
    def __init__(self, rxnIRI, rxn_conditions: list, perf_indicators: list, query_endpoint, update_endpoint):
        """
            Initialises the information to be used for creating OntoRxn:ReactionVariation instance.

            Arguments:
                rxnIRI - IRI of the OntoRxn:ReactionExperiment instance which the created OntoRxn:ReactionVariation should be attached to
                rxn_conditions - a list of reaction conditions to be added into the instance of OntoRxn:ReactionVariation to be created
                perf_indicators - a list of reaction performance indicator to be computed once the OntoRxn:ReactionVariation is conducted
                query_endpoint - SPARQL Query endpoint for knowledge graph
                update_endpoint - SPARQL Update endpoint for knowledge graph
        """
        # IRI-ise the reaction experiment instance to be used by rdflib package
        self.rIRI = URIRef(rxnIRI)
        # Initialise the IRI of OntoRxn:ReactionVariation instance with empty string
        self.rxnvar = ""
        # Assign reaction conditions and performance indicators
        self.conditions = rxn_conditions
        self.indicators = perf_indicators
        # Initialise a rdflib Graph() as the basis of OntoRxn:ReactionVariation instance
        self.g = Graph()
        # Assign SPARQL Query and Update endpoint for knowledge graph
        self.query = query_endpoint
        self.update = update_endpoint
    
    def uploadOntoRxnInstanceToKG(self):
        """
            Upload the created OntoRxn:ReactionVariation instance to the knowledge graph.
            All information should already be prepared and added to the instance of rdflib Graph by this point.
        """
        # Generate a file path that is used to store the created OntoRxn:ReactionVariation instance
        filePath = f'{str(uuid.uuid4())}.xml'
        # Serialise the created OntoRxn:ReactionVariation instance as a XML file
        self.g.serialize(filePath, format='xml')
        # Upload the created OntoRxn:ReactionVariation instance to knowledge graph
        uploadOntology(TRIPLE_STORE_UPLOAD_SERVER, TRIPLE_STORE_UPLOAD_REPOSITORY, filePath)
        # os.remove(filePath)
    
    def createOntoRxnInstance(self):
        """
            Create OntoRxn:ReactionVariation instance using all collected information.
        """
        # Generate and IRI-ise the IRI of OntoRxn:ReactionVariation instance to be used by rdflib package
        self.rxnvar = URIRef(NAMESPACE_KB_ONTORXN + getShortName(ONTORXN_REACTIONVARIATION) + '_' + str(uuid.uuid4()))
        
        # Add below triples:
        # <reactionVariationIRI> <rdf:type> <OntoRxn:ReactionVariation> .
        # <reactionVariationIRI> <OntoRxn:isVariationOf> <reactionExperimentIRI> .
        # <reactionExperimentIRI> <OntoRxn:hasVariation> <reactionVariationIRI> .
        self.g.add((self.rxnvar, RDF.type, URIRef(ONTORXN_REACTIONVARIATION)))
        self.g.add((self.rxnvar, URIRef(ONTORXN_ISVARIATIONOF), self.rIRI))
        self.g.add((self.rIRI, URIRef(ONTORXN_HASVARIATION), self.rxnvar))

        # Iterate over the list of reaction conditions to be added and add each condition
        for con in self.conditions:
            self.addReactionCondition(**con)
        # Iterate over the list of performance indicators to be added and add each indicator
        for ind in self.indicators:
            self.addPerformanceIndicator(**ind)
        
        return str(self.rxnvar)

    def addReactionCondition(self, clz, num, id=None, **kwargs):
        """
            Add single reaction condition to the OntoRxn:ReactionVariation instance.

            Arguments:
                clz - the rdf:type (complete IRI) of the reaction condition to be added
                num - the numerical value of the reaction condition
                id - positional ID of the reaction condition if any (to be used for uniquely identifying the instance across different reaction experiments/variations in design of experiment exercise)
                **kwargs - used to accommodate any unhandled additonal keys from the input
        """
        # Generate and IRI-ise the IRI of reaction condition instance to be used by rdflib package
        condition_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(clz) + '_' + str(uuid.uuid4()))
        # Generate and IRI-ise the IRI of om:Measure instance to be used by rdflib package
        measure_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(OM_MEASURE) + '_' + str(uuid.uuid4()))

        # Attach the reaction condition instance to the OntoRxn:ReactionVariation instance 
        # As we are stating the <reactionVariationIRI> <OntoRxn:isVariationOf> <reactionExperimentIRI>
        # we are using the list of the object properties between the <reactionExperimentIRI> and the class which the reaction condition belongs to
        for pred in getObjectRelationship(self.query, self.rIRI, clz):
            self.g.add((self.rxnvar, URIRef(pred), condition_iri))
        
        # Add below triples following units of measure practices:
        # <reactionConditionIRI> <rdf:type> <clz> .
        # <reactionConditionIRI> <om:hasPhenomenon> <reactionVariationIRI> .
        # <reactionConditionIRI> <om:hasValue> <measureIRI> .
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
        
        # Add below triples following units of measure practices:
        # <measureIRI> <rdf:type> <om:Measure> .
        # <measureIRI> <om:hasUnit> <unit> .
        # <measureIRI> <om:hasNumericalValue> <val> .
        self.g.add((measure_iri, RDF.type, URIRef(OM_MEASURE)))
        # The added unit is retrieved from the original OntoRxn:ReactionExperiment instance
        if id is not None:
            self.g.add((measure_iri, URIRef(OM_HASUNIT), URIRef(getQuantityUnit(self.query, str(self.rIRI), clz, int(id)))))
        else:
            self.g.add((measure_iri, URIRef(OM_HASUNIT), URIRef(getQuantityUnit(self.query, str(self.rIRI), clz, None))))
        self.g.add((measure_iri, URIRef(OM_HASNUMERICALVALUE), Literal(float(num))))

    def addPerformanceIndicator(self, clz, id=None, **kwargs):
        """
            Add single performance indicator to the OntoRxn:ReactionVariation instance.

            Arguments:
                clz - the rdf:type (complete IRI) of the performance indicator to be added
                id - positional ID of the performance indicator if any (to be used for uniquely identifying the instance across different reaction experiments/variations in design of experiment exercise)
                **kwargs - used to accommodate any unhandled additonal keys from the input
        """
        # Create performance indicator instance
        indicator_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(clz) + '_' + str(uuid.uuid4()))
        # Add performance indicator to reaction experiment (or variation)
        # As we are stating the <reactionVariationIRI> <OntoRxn:isVariationOf> <reactionExperimentIRI>
        # we are using the list of the object properties between the <reactionExperimentIRI> and the class which the performance indicator belongs to
        for pred in getObjectRelationship(self.query, self.rIRI, clz):
            self.g.add((self.rxnvar, URIRef(pred), indicator_iri))
        
        # The previously added below statement is seen to be not suitable given the latest design of OntoRxn TBox, hence commented out
        # <reactionVariationIRI> <OntoRxn:hasPerformanceIndicator> <indicator_iri>
        # self.g.add((self.rxnvar, URIRef(ONTORXN_HASPERFORMANCEINDICATOR), indicator_iri))

        self.g.add((indicator_iri, URIRef(OM_HASPHENOMENON), self.rxnvar))
        # Add rdf:type to created performance indicator instance
        self.g.add((indicator_iri, RDF.type, URIRef(clz)))
        # Only add positional ID if it exists
        if id is not None:
            self.g.add((indicator_iri, URIRef(ONTODOE_POSITIONALID), Literal(int(id))))

def uploadNewExpToKG(doe, next_exp: DataSet):
    """
        This method is used to populate the suggested new experiments back to the knowledge graph. 
        It firstly creates and uploads the OntoRxn:ReactionVariation instances. 
        It then creates and uploads the OntoDoE:NewExperiment instance. 
        The method returns IRI of the created OntoDoE:NewExperiment instance.

        Arguments:
            doe - a dict of information about design of experiment exercise, mandatory keys: 'first_exp', 'continuousVariables', 'systemResponses', 'doe_instance'
                  
                  an example of doe['first_exp']: 'https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1'

                  an example of doe['continuousVariables']: 
                  [{"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#StoichiometryRatio', "id": 2, "num": 3}, 
                  {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#StoichiometryRatio', "id": 2, "num": 0.05}, 
                  {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#ReactionTemperature', "num": 60}, 
                  {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#ResidenceTime', "num": 5}, ]
                  
                  an example of doe['systemResponses']:
                  [{"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#Yield'}, 
                  {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#RunMaterialCost', "id": 7},
                  {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#SpaceTimeYield', "id": 8, "num": 0.5}]

                  an example of doe['doe_instance']: 'https://theworldavatar.com/kb/ontodoe/DoE_1/DoE_1'

            new_exp - a DataSet of new suggested experiments
    """

    # Initialise a list to host the new suggested OntoRxn:ReactionVariation instances
    new_exp_iri_list = []
    
    # Iterate over the new suggested experiments to create and upload each of them to the knowledge graph
    for i in range(len(next_exp)):
        # Retrieve all the new suggested reaction conditions and put that together with other design of experiment information
        for var in doe['continuousVariables']:
            var['num'] = next_exp[var['name']][i] # an example: df['ContinuousVariable_1'][0]
        
        # Initialise the ReactionVariation class
        new_exp_ = ReactionVariation(doe['first_exp'], doe['continuousVariables'], doe['systemResponses'], SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT)
        # Create the OntoRxn:ReactionVariation instance
        new_exp_iri = new_exp_.createOntoRxnInstance()
        # Upload the OntoRxn:ReactionVariation instance to the knowledge graph
        new_exp_.uploadOntoRxnInstanceToKG()
        # Update the list of new suggested OntoRxn:ReactionVariation instances
        new_exp_iri_list.append(new_exp_iri)
    
    # Create and upload the OntoDoE:NewExperiment instance to the knowledge graph
    ontodoe_new_exp_iri = createOntoDoENewExperimentIRI(SPARQL_UPDATE_ENDPOINT, doe['doe_instance'], new_exp_iri_list)
    
    return ontodoe_new_exp_iri
