################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 30 Nov 2022                            #
################################################

# The purpose of this module is to instantiate/update the potential impact of a flood 
# by estimating the affected population and buildings by a particular flood alert/warning
# instantiated in the KG (using asynchronous derivation framework)

import uuid
import pandas as pd
from rdflib import Graph

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from floodassessment.datamodel.iris import *
from floodassessment.kg_operations.kgclient import KGClient
from floodassessment.utils.stackclients import PostGISClient


class FloodAssessmentAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)
        

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        return [FLOOD_ALERT_WARNING, OBE_BUILDING, OM_AMOUNT_MONEY]


    def agent_output_concepts(self) -> list:
        # Output concept (i.e. result) of the Derivation
        return [FLOOD_POPULATION, FLOOD_BUILDING, FLOOD_IMPACT]


    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)


    def validate_input_values(self, inputs, derivationIRI=None):
        """
        Check whether received input values are suitable to perform flood impact assessment.
        Throw exception if data is not suitable -> relevant for asynchronous derivation

        Arguments:
            inputs {dict} -- Dictionary of inputs with input concepts as keys and values as list
            derivationIRI {str} -- IRI of the derivation instance (optional)

        Returns:
            flood_alert_warning {str}, 
            building_iris {list}, value_estimation_iris {list}
        """

        # Verify that exactly one flood alert/warning instance is provided
        inp = inputs.get(FLOOD_ALERT_WARNING)
        if len(inp) == 1:
            flood_alert_warning = inp[0]
        else:
            self.logger.error(f"Derivation {derivationIRI}: More than one 'FloodAlertOrWarning' IRI provided.")
            raise Exception(f"Derivation {derivationIRI}: More than one 'FloodAlertOrWarning' IRI provided.")

        # Extract lists of building IRIs and value estimation IRIs
        # (i.e. in principle both lists could be empty without causing issues)
        building_iris = inputs.get(OBE_BUILDING)
        value_estimation_iris = inputs.get(OM_AMOUNT_MONEY)
        # Create empty lists in case no buildings or value estimations have been marked up
        building_iris = [] if building_iris is None else building_iris
        value_estimation_iris = [] if value_estimation_iris is None else value_estimation_iris

        if len(value_estimation_iris) > len(building_iris):
            self.logger.error(f"Derivation {derivationIRI}: More value estimations than buildings provided.")
            raise Exception(f"Derivation {derivationIRI}: More value estimations than buildings provided.")

        return flood_alert_warning, building_iris, value_estimation_iris

    
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        """
        This method takes 
            1 IRI of RT:FloodALertOrWarning
            1 List of OBE:Building IRIs (could be empty)
            1 List of OM:AmountMoney IRIs (could be empty)
        to assess the estimated impact of a potential flood and generate
            1 IRI of Flood:Impact
            1 IRI of Flood:Population
            1 IRI of Flood:Building
            (including the respective instantiation of OM:AmountOfMoney + full
             set of triples due to ontology of units of measure representation)
        """

        # Get input IRIs from the agent inputs (derivation_inputs)
        # (returns dict of inputs with input concepts as keys and values as list)
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()
        warning, buildings, estimations = self.validate_input_values(inputs=inputs,
                                               derivationIRI=derivIRI)
        
        # Assess potential flood impacts in case all required inputs are available
        # (i.e. relevant inputs have been marked up successfully)
        g = self.estimate_potential_flood_impact(warning_iri=warning,
                                                 building_iris=buildings, 
                                                 estimation_iris=estimations)       

        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)


    def estimate_number_of_affected_people(self, flood_alert_warning_iri: str) -> int:
        """
        Estimate the number of "affected" people by a flood alert/warning using 
        PostGIS' geospatial count over population density raster data within the
        boundary of the ArealExtendPolygon associated with the flood alert/warning
        """

        # Initialise relevant PostGIS client
        postgis_client = PostGISClient()

        # Get flood area IRI associated with flood alert/warning
        area_iri = self.sparql_client.get_associated_flood_area(flood_alert_warning_iri)

        # Get number of affected population
        # Returns int (number of affected people) or None (in case if no population data
        # could be determined, i.e. nobody is affected or specified area is not present)
        affected = postgis_client.get_number_of_affected_population(flood_area_iri=area_iri)
        
        return affected


    def estimate_potential_flood_impact(self, warning_iri:str = None,
                                        building_iris:list = None, 
                                        estimation_iris:list = None):
        """
        Estimate number of people and buildings affected by a flood alert/warning
        and create corresponding (derivation) output triples (as graph)

        Arguments:
            warning_iri {str} - IRI of RT:FloodALertOrWarning
            building_iris {str} - List of OBE:Building IRIs
            estimation_iris {str} - List of OM:AmountMoney IRIs
        Returns:
            Graph to instantiate/update property market value
        """

        # Initialise return graph
        g = Graph()

        # Check severity (i.e. status) of flood alert/warning
        severity = self.sparql_client.get_flood_alert_warning_severity(warning_iri)
        if severity == FLOOD_SEVERITY_INACTIVE:
            # If flood alert/warning is inactive --> set impact to zero
            affected_population = 0
            affected_buildings_count = 0
            affected_buildings_value = 0
            self.logger.info(f"Flood alert/warning '{warning_iri}' marked as inactive.")
        else:
            # If flood alert/warning is active --> assess impact
            affected_population = self.estimate_number_of_affected_people(warning_iri)
            affected_buildings_count = len(building_iris)
            affected_buildings_value = self.sparql_client.summarise_affected_property_values(estimation_iris)

        # Retrieve potentially already instantiated flood instance
        # (if None if retrieved, new Flood instance will be created by derivation)
        flood_iri = self.sparql_client.get_associated_flood_event(warning_iri)

        # Generate textual description of potential impact
        impact_description = self.create_impact_description(severity)

        # Create triples to instantiate flood impact assessment
        g = self.sparql_client.instantiate_flood_impact(graph=g,
                                    flood_alert_warning_iri=warning_iri,
                                    affected_buildings_count=affected_buildings_count,
                                    affected_buildings_value=affected_buildings_value,
                                    affected_population=affected_population,
                                    impact_description=impact_description,
                                    flood_iri=flood_iri)

        # Return graph with SPARQL update (empty for unavailable market value)
        return g


    def create_impact_description(self, severity_iri):
        """
        Create textual description of flood impact, currently based on:
        https://environment.data.gov.uk/flood-monitoring/doc/reference#introduction
        """
        # Initialise return value (for inactive flood alert/warning)
        impact_description = None

        if severity_iri == FLOOD_SEVERITY_SEVERE:
            impact_description = "Severe Flooding, Danger to Life."
        elif severity_iri == FLOOD_SEVERITY_WARNING:
            impact_description = "Flooding is Expected, Immediate Action Required."
        elif severity_iri == FLOOD_SEVERITY_ALERT:
            impact_description = "Flooding is Possible, Be Prepared."
        
        return impact_description


def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent to estimate the potential damage of a flood based on instantiated Flood Alerts and Warnings with regards to <BR>"
    msg += "1) the number of affected people (i.e. people living in flood polygon associated with raised alert/warning) as well as"
    msg += "2) the number and estimated value of affected buildings (i.e. number and cumulative value of buildings located in flood polygon).<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FloodAssessmentAgent<BR>"
    return msg
