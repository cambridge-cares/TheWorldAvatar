# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from rdflib import Graph, URIRef, Namespace, Literal, BNode
from rdflib.namespace import RDF
import pandas as pd
import collections
import json
import time
import uuid
import os

from pyasyncagent.kg_operations import PySparqlClient
from pyasyncagent.data_model import *

from chemistry_and_robots.data_model import *

import logging
logger = logging.getLogger('chemistry_and_robots_sparql_client')
logging.getLogger('py4j').setLevel(logging.INFO)

class ChemistryAndRobotsSparqlClient(PySparqlClient):
    def updateNewExperimentInKG(self, doe: DesignOfExperiment, newExp: List[ReactionExperiment]):
        """
            This method is used to populate the suggested new experiments back to the knowledge graph.
            It firstly first serialise and upload ReactionVariation/ReactionExperiment instance to the knowledge graph.
            It then replace the link between the OntoDoE:DesignOfExperiment instance and the old OntoRxn:ReactionExperiment/ReactionVariation instance with the new created OntoRxn:ReactionExperiment/ReactionVariation instance.

            Arguments:
                doe - instance of dataclass OntoDoE.DesignOfExperiment
                newExp - a list of instance of dataclass OntoRxn.ReactionExperiment
        """
        # (1) first serialise and upload ReactionVariation/ReactionExperiment instance to KG
        # Generate a file path that is used to store the created OntoRxn:ReactionVariation instance
        filePath = f'{str(uuid.uuid4())}.ttl'
        # Serialise the created OntoRxn:ReactionVariation/ReactionExperiment instance as a XML file
        # All information should already be prepared and added to the instance
        # Method create_instance_for_kg will write all information to rdflib Graph on-the-fly
        g = Graph()
        # NOTE although here we loop through the list of OntoRxn:ReactionVariation/ReactionExperiment
        # NOTE in theory, the len(newExp) should be 1 (as we decided to make DoE Agent only suggest 1 experiment per derivation)
        # NOTE the loop is added for the future development
        for exp in newExp:
            g = exp.create_instance_for_kg(g)
        g.serialize(filePath, format='ttl')
        # Upload the created OntoRxn:ReactionVariation/ReactionExperiment instance to knowledge graph
        self.uploadOntology(filePath)
        # Delete generated Turtle file
        os.remove(filePath)

        # (2) replace connection between OntoDoE:DesignOfExperiment with OntoDoE:NewExperiment
        # Construct SPARQL Update string
        # delete existing <DoE> <proposesNewExperiment> <newExp_old>
        # add <DoE> <proposesNewExperiment> <newExp>
        # NOTE here the for loop is added due to the same reason - for the future development
        update = """DELETE {<%s> <%s> ?newexp .}""" % (doe.instance_iri, ONTODOE_PROPOSESNEWEXPERIMENT)
        for exp in newExp:
            update += """INSERT {<%s> <%s> <%s> .} """ % (doe.instance_iri, ONTODOE_PROPOSESNEWEXPERIMENT, exp.instance_iri)
        update += """WHERE {<%s> <%s> ?newexp .}""" % (doe.instance_iri, ONTODOE_PROPOSESNEWEXPERIMENT)

        # Perform SPARQL Update
        self.performUpdate(update)

    def getDoEInstanceIRI(self, strategy_instance: Strategy, domain_instance: Domain, system_response_instance: List[SystemResponse], historical_data_instance: HistoricalData) -> str:
        """
            This method retrieves the instance of OntoDoE:DesignOfExperiment given instance of OntoDoE:Strategy, OntoDoE:Domain, OntoDoE:SystemResponse, and OntoDoE:HistoricalData.

            Arguments:
                doe_instance - instance of dataclass OntoDoE.DesignOfExperiment
        """

        # Prepare query string, start with strategy and domain, then iterate over a list of system responses, finally historical data
        query = """SELECT ?doe_instance \
                WHERE { \
                ?doe_instance <%s> <%s> ; \
                    <%s> <%s> ; """ % (ONTODOE_USESSTRATEGY, strategy_instance.instance_iri, ONTODOE_HASDOMAIN, domain_instance.instance_iri)

        for sysres in system_response_instance:
            query = query + """<%s> <%s> ; """ % (ONTODOE_HASSYSTEMRESPONSE, sysres.instance_iri)
        query = query + """<%s> <%s> . }""" % (ONTODOE_UTILISESHISTORICALDATA, historical_data_instance.instance_iri)
        # Perform query
        response = self.performQuery(query)
        if (len(response) == 0 ):
            raise Exception("""Unable to identify the OntoDoE:DesignOfExperiment instance given input: \
                OntoDoE:Strategy <%s>; \
                OntoDoE:Domain <%s>; \
                OntoDoE:SystemResponse <%s>; \
                OntoDoE:HistoricalData <%s>.""" % (strategy_instance.instance_iri, domain_instance.instance_iri, ">, <".join([sysres.instance_iri for sysres in system_response_instance]), historical_data_instance.instance_iri))
        elif (len(response) > 1):
            raise Exception("""Unable to uniquely identify the OntoDoE:DesignOfExperiment instance given input: \
                OntoDoE:Strategy <%s>; \
                OntoDoE:Domain <%s>; \
                OntoDoE:SystemResponse <%s>; \
                OntoDoE:HistoricalData <%s>. \
                The list of identified OntoDoE:DesignOfExperiment instances are: <%s>.""" % (strategy_instance.instance_iri, domain_instance.instance_iri, ">, <".join([sysres.instance_iri for sysres in system_response_instance]), historical_data_instance.instance_iri, ">, <".join([list(r.values())[0] for r in response])))
        else:
            return response[0]['doe_instance']

    def getDoEDomain(self, domain_iri: str) -> Domain:
        """
            This method retrieves information given instance iri of OntoDoE:Domain.

            Arguments:
                domain_iri - iri of OntoDoE:Domain instance
        """
        # Delete "<" and ">" around the IRI
        domain_iri = trimIRI(domain_iri)
        domain_instance = Domain(instance_iri=domain_iri, hasDesignVariable=self.getDesignVariables(domain_iri))
        return domain_instance

    def getDesignVariables(self, domain_iri: str) -> List[DesignVariable]:
        """
            This methods retrieves all the design variables pointed by the iri of the given instance of dataclass OntoDoE.Domain.

            Arguments:
                domain_iri - iri of OntoDoE:Domain instance
        """
        # Delete "<" and ">" around the IRI
        domain_iri = trimIRI(domain_iri)

        # TODO add support for CategoricalVariable
        # Prepare query string
        query = """SELECT DISTINCT ?var ?clz ?id ?lower ?upper \
                WHERE { \
                    <%s> <%s> ?var . \
                    ?var <%s> ?clz . \
                    OPTIONAL {?var <%s> ?id . } \
                    OPTIONAL {?var <%s> ?lower . } \
                    OPTIONAL {?var <%s> ?upper . } \
                }""" % (domain_iri, ONTODOE_HASDESIGNVARIABLE, ONTODOE_REFERSTO, ONTODOE_POSITIONALID, ONTODOE_LOWERLIMIT, ONTODOE_UPPERLIMIT)
        
        # Perform query
        response = self.performQuery(query)
        
        # Construct list of design variables
        list_var = []
        for res in response:
            list_var.append(
                ContinuousVariable(
                    instance_iri=res['var'],
                    name=getShortName(res['var']),
                    upperLimit=res['upper'],
                    lowerLimit=res['lower'],
                    positionalID=res['id'] if 'id' in res else None,
                    refersTo=res['clz']
                )
            )

        return list_var

    def getSystemResponses(self, systemResponse_iris) -> List[SystemResponse]:
        """
            This methods retrieves all the system responses given iris of OntoDoE:SystemResponses.

            Arguments:
                systemResponse_iris - a list of OntoDoE:SystemResponse iri
        """
        # Delete "<" and ">" around the IRI
        systemResponse_iris = trimIRI(systemResponse_iris)
        
        # Add safeguard in case the input systemResponse_iris is not a list
        if not isinstance(systemResponse_iris, list):
            systemResponse_iris = [systemResponse_iris]
        
        list_sys = []
        for sys_ins in systemResponse_iris:
            # Prepare query string
            query = """SELECT DISTINCT ?clz ?id ?maximise \
                    WHERE { \
                    <%s> <%s> ?clz . 
                    OPTIONAL {<%s> <%s> ?id} \
                    OPTIONAL {<%s> <%s> ?maximise} \
                    }""" % (sys_ins, ONTODOE_REFERSTO, sys_ins, ONTODOE_POSITIONALID, sys_ins, ONTODOE_MAXIMISE)
            # Perform query
            response = self.performQuery(query)
            list_sys.append(
                SystemResponse(
                    instance_iri=sys_ins,
                    name=getShortName(sys_ins),
                    maximise=response[0]['maximise'],
                    positionalID=response[0]['id'] if 'id' in response[0] else None,
                    refersTo=response[0]['clz']
                )
            )
            
        return list_sys

    def getDoEHistoricalData(self, historicalData_iri: str) -> HistoricalData:
        """
            This method retrieves information given instance iri of OntoDoE:HistoricalData.

            Arguments:
                historicalData_iri - iri of OntoDoE:HistoricalData instance
        """
        # Delete "<" and ">" around the IRI
        historicalData_iri = trimIRI(historicalData_iri)

        query = """SELECT DISTINCT ?exp ?numOfNewExp \
                WHERE { \
                <%s> <%s> ?exp . \
                OPTIONAL {<%s> <%s> ?numOfNewExp .} \
                }""" % (historicalData_iri, ONTODOE_REFERSTO, historicalData_iri, ONTODOE_NUMOFNEWEXP)
        
        response = self.performQuery(query)

        num_ = [res['numOfNewExp'] for res in response]
        if num_.count(num_[0]) != len(num_):
            raise Exception("There are multiple instances of numOfNewExp associated with OntoDoE:HistoricalData instance <" + historicalData_iri + ">: " + collections.Counter(num_).keys)

        rxnexp_iris = [res['exp'] for res in response]

        historicalData_instance = HistoricalData(
            instance_iri=historicalData_iri,
            numOfNewExp=num_[0], # TODO what if no instance is in place?
            refersTo=self.getReactionExperiment(rxnexp_iris)
            )
        return historicalData_instance

    def getReactionExperiment(self, rxnexp_iris: str or list) -> List[ReactionExperiment]:
        """
            This method retrieves information given a list of instance iri of OntoRxn:ReactionExperiment.

            Arguments:
                rxnexp_iris - iri of OntoRxn:ReactionExperiment instance
        """

        # TODO implement logic of querying information for OntoRxn:ReactionExperiment (most importantly parsing InputChemical and OutputChemical)
        if not isinstance(rxnexp_iris, list):
            rxnexp_iris = [rxnexp_iris]

        list_exp = []
        for exp_iri in rxnexp_iris:
            rdf_type_rxn = self.get_rdf_type_of_rxn_exp(exp_iri)
            if rdf_type_rxn == ONTORXN_REACTIONEXPERIMENT:
                list_exp.append(
                    ReactionExperiment(
                        instance_iri=exp_iri,
                        hasReactionCondition=self.getExpReactionCondition(exp_iri),
                        hasPerformanceIndicator=self.getExpPerformanceIndicator(exp_iri),
                        # TODO add support for parsing InputChemical and OutputChemical
                        hasInputChemical=self.get_input_chemical_of_rxn_exp(exp_iri),
                        hasOutputChemical=self.get_output_chemical_of_rxn_exp(exp_iri),
                        isAssignedTo=self.get_r4_reactor_rxn_exp_assigned_to(exp_iri)
                    )
                )
            elif rdf_type_rxn == ONTORXN_REACTIONVARIATION:
                list_exp.append(
                    ReactionVariation(
                        instance_iri=exp_iri,
                        hasReactionCondition=self.getExpReactionCondition(exp_iri),
                        hasPerformanceIndicator=self.getExpPerformanceIndicator(exp_iri),
                        # TODO add support for parsing InputChemical and OutputChemical
                        hasInputChemical=self.get_input_chemical_of_rxn_exp(exp_iri),
                        hasOutputChemical=self.get_output_chemical_of_rxn_exp(exp_iri),
                        isAssignedTo=self.get_r4_reactor_rxn_exp_assigned_to(exp_iri)
                    )
                )
        return list_exp

    def get_rdf_type_of_rxn_exp(self, rxnexp_iri: str) -> str:
        rxnexp_iri = trimIRI(rxnexp_iri)
        query = PREFIX_RDF + """SELECT ?type WHERE { <%s> rdf:type ?type. filter(?type in (<%s>, <%s>)) }""" % (rxnexp_iri, ONTORXN_REACTIONEXPERIMENT, ONTORXN_REACTIONVARIATION)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple rdf:type identified for reaction experiment <%s>: %s" % (rxnexp_iri, str(response)))
        elif len(response) < 1:
            raise Exception("Reaction experiment <%s> is missing rdf:type as either OntoRxn:ReactionExperiment or OntoRxn:ReactionVariation." % (rxnexp_iri))
        else:
            return response[0]['type']

    def get_input_chemical_of_rxn_exp(self, rxnexp_iri: str) -> List[InputChemical]:
        rxnexp_iri = trimIRI(rxnexp_iri)
        return self.get_ontocape_material(rxnexp_iri, ONTORXN_HASINPUTCHEMICAL)

    def get_output_chemical_of_rxn_exp(self, rxnexp_iri: str) -> List[OutputChemical]:
        rxnexp_iri = trimIRI(rxnexp_iri)
        return self.get_ontocape_material(rxnexp_iri, ONTORXN_HASOUTPUTCHEMICAL)

    def get_ontocape_material(self, subject_iri, predicate_iri) -> List[OntoCAPE_Material]:
        subject_iri = trimIRI(subject_iri)
        predicate_iri = trimIRI(predicate_iri)

        if predicate_iri == ONTORXN_HASINPUTCHEMICAL:
            ocm_query_key = 'input_chemical'
        elif predicate_iri == ONTORXN_HASOUTPUTCHEMICAL:
            ocm_query_key = 'output_chemical'
        else:
            ocm_query_key = 'ontocape_material'

        query = PREFIX_RDF + \
                """
                SELECT ?%s ?single_phase ?state_of_aggregation ?composition ?phase_component ?chemical_species ?phase_component_concentration ?concentration_type ?value ?unit ?num_val
                WHERE {
                    <%s> <%s> ?%s .
                    ?%s <%s> ?single_phase .
                    ?single_phase rdf:type <%s> .
                    ?single_phase <%s> ?state_of_aggregation; <%s> ?composition; <%s> ?phase_component .
                    ?composition <%s> ?phase_component_concentration .
                    ?phase_component <%s> ?chemical_species .
                    ?phase_component <%s> ?phase_component_concentration .
                    ?phase_component_concentration rdf:type ?concentration_type; <%s> ?value .
                    ?value <%s> ?unit; <%s> ?num_val.
                }
                """ % (
                    ocm_query_key, subject_iri, predicate_iri, ocm_query_key, ocm_query_key,
                    ONTOCAPE_THERMODYNAMICBEHAVIOR, ONTOCAPE_SINGLEPHASE,
                    ONTOCAPE_HASSTATEOFAGGREGATION, ONTOCAPE_HAS_COMPOSITION, ONTOCAPE_ISCOMPOSEDOFSUBSYSTEM,
                    ONTOCAPE_COMPRISESDIRECTLY, ONTOCAPE_REPRESENTSOCCURENCEOF, ONTOCAPE_HASPROPERTY,
                    ONTOCAPE_HASVALUE, ONTOCAPE_HASUNITOFMEASURE, ONTOCAPE_NUMERICALVALUE
                )

        response = self.performQuery(query)

        # NOTE here we make that if nothing found, return None
        if len(response) == 0:
            logger.warning("Nothing found when quering: %s" % (query))
            return None
        else:
            lst_ontocape_material = []

            # generate different list for each OntoCAPE:Material
            unique_ontocape_material_iri = self.get_unique_values_in_list_of_dict(response, ocm_query_key)
            list_list_ontocape_material = []
            for iri in unique_ontocape_material_iri:
                list_list_ontocape_material.append([res for res in response if iri == res[ocm_query_key]])

            for list_om in list_list_ontocape_material:
                ontocape_material_iri = self.get_unique_values_in_list_of_dict(list_om, ocm_query_key)[0] # here we are sure this is the unique value of OntoCAPE:Material

                # validate that the list of responses are only referring to one instance of OntoCAPE_SinglePhase, one instance of OntoCAPE_StateOfAggregation and one instance of OntoCAPE_Composition, otherwise raise an Exception
                unique_single_phase_iri = self.get_unique_values_in_list_of_dict(list_om, 'single_phase')
                if len(unique_single_phase_iri) > 1:
                    raise Exception("Multiple thermodynamicBehavior OntoCAPE:SinglePhase identified (<%s>) in one instance of OntoRxn:InputChemical/OntoRxn:OutputChemical/OntoCAPE:Material %s is currently NOT supported." % ('>, <'.join(unique_single_phase_iri), ontocape_material_iri))
                elif len(unique_single_phase_iri) < 1:
                    raise Exception("No instance of thermodynamicBehavior OntoCAPE:SinglePhase was identified given instance of OntoRxn:InputChemical/OntoRxn:OutputChemical/OntoCAPE:Material: %s" % (ontocape_material_iri))
                else:
                    unique_single_phase_iri = unique_single_phase_iri[0]

                unique_state_of_aggregation_iri = self.get_unique_values_in_list_of_dict(list_om, 'state_of_aggregation')
                if len(unique_state_of_aggregation_iri) > 1:
                    raise Exception("Multiple hasStateOfAggregation OntoCAPE:StateOfAggregation identified (<%s>) in one instance of OntoCAPE:SinglePhase %s is currently NOT supported." % ('>, <'.join(unique_state_of_aggregation_iri), unique_single_phase_iri))
                elif len(unique_state_of_aggregation_iri) < 1:
                    raise Exception("No instance of hasStateOfAggregation OntoCAPE:StateOfAggregation was identified given instance of OntoCAPE:SinglePhase: %s" % (unique_single_phase_iri))
                else:
                    if unique_state_of_aggregation_iri[0] == ONTOCAPE_LIQUID:
                        state_of_aggregation = OntoCAPE_liquid
                    else:
                        # TODO add support for other phase (solid, gas)
                        pass

                unique_composition_iri = self.get_unique_values_in_list_of_dict(list_om, 'composition')
                if len(unique_composition_iri) > 1:
                    raise Exception("Multiple has_composition OntoCAPE:Composition identified (<%s>) in one instance of OntoCAPE:SinglePhase %s is currently NOT supported." % ('>, <'.join(unique_composition_iri), unique_single_phase_iri))
                elif len(unique_composition_iri) < 1:
                    raise Exception("No instance of has_composition OntoCAPE:Composition was identified given instance of OntoCAPE:SinglePhase: %s" % (unique_single_phase_iri))
                else:
                    unique_composition_iri = unique_composition_iri[0]

                # secondly, get a list of OntoCAPE_PhaseComponent to be added to the OntoCAPE_SinglePhase instance
                list_phase_component = []
                list_phase_component_concentration = []
                for r in list_om:
                    if 'concentration_type' in r:
                        if r['concentration_type'] == OntoCAPE_Molarity.__fields__['clz'].default:
                            concentration = OntoCAPE_Molarity(instance_iri=r['phase_component_concentration'],hasValue=OntoCAPE_ScalarValue(instance_iri=r['value'],numericalValue=r['num_val'],hasUnitOfMeasure=r['unit']))
                        else:
                            # TODO add support for other type of OntoCAPE_PhaseComponentConcentration
                            pass
                        list_phase_component_concentration.append(concentration)
                    else:
                        raise Exception("Concentration is not defined for")

                    phase_component = OntoCAPE_PhaseComponent(instance_iri=r['phase_component'],representsOccurenceOf=r['chemical_species'],hasProperty=concentration)
                    list_phase_component.append(phase_component)

                composition = OntoCAPE_Composition(
                    instance_iri=unique_composition_iri,
                    comprisesDirectly=list_phase_component_concentration
                )
                single_phase = OntoCAPE_SinglePhase(
                    instance_iri=unique_single_phase_iri,
                    hasStateOfAggregation=state_of_aggregation,
                    isComposedOfSubsystem=list_phase_component,
                    has_composition=composition,
                    representsThermodynamicBehaviorOf=ontocape_material_iri
                )

                if predicate_iri == ONTORXN_HASINPUTCHEMICAL:
                    ocm_instance = InputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                elif predicate_iri == ONTORXN_HASOUTPUTCHEMICAL:
                    ocm_instance = OutputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                else:
                    ocm_instance = OntoCAPE_Material(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)

                lst_ontocape_material.append(ocm_instance)

            return lst_ontocape_material

    def get_all_autosampler_with_fill(self, given_autosampler_iri: str = None) -> List[AutoSampler]:
        if given_autosampler_iri == None:
            query = PREFIX_RDF + \
                    """
                    SELECT ?autosampler ?autosampler_manufacturer ?laboratory ?autosampler_power_supply
                    ?site ?loc ?vial ?fill_level ?fill_level_om_value ?fill_level_unit ?fill_level_num_val
                    ?max_level ?max_level_om_value ?max_level_unit ?max_level_num_val ?chemical_solution
                    WHERE {
                        ?autosampler rdf:type <%s>; <%s> ?autosampler_manufacturer; <%s> ?laboratory; <%s> ?autosampler_power_supply.
                        ?autosampler <%s> ?site.
                        ?site <%s> ?vial; <%s> ?loc.
                        OPTIONAL {
                            ?vial <%s> ?fill_level.
                            ?fill_level <%s> ?fill_level_om_value.
                            ?fill_level_om_value <%s> ?fill_level_unit; <%s> ?fill_level_num_val.
                        }
                        OPTIONAL {
                            ?vial <%s> ?max_level.
                            ?max_level <%s> ?max_level_om_value.
                            ?max_level_om_value <%s> ?max_level_unit; <%s> ?max_level_num_val.
                        }
                        ?vial <%s> ?chemical_solution.
                    }
                    """ % (
                        ONTOVAPOURTEC_AUTOSAMPLER, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY,
                        ONTOVAPOURTEC_HASSITE, ONTOVAPOURTEC_HOLDS, ONTOVAPOURTEC_LOCATIONID,
                        ONTOVAPOURTEC_HASFILLLEVEL, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                        ONTOVAPOURTEC_HASMAXLEVEL, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                        ONTOVAPOURTEC_ISFILLEDWITH
                    )

            response = self.performQuery(query)

            unique_autosampler_list = self.get_unique_values_in_list_of_dict(response, 'autosampler')
            logger.debug("The list of all available OntoVapourtec:AutoSampler in the knowledge graph (%s): %s" % (self.kg_client.getQueryEndpoint(), str(unique_autosampler_list)))
        else:
            given_autosampler_iri = trimIRI(given_autosampler_iri)
            query = PREFIX_RDF + \
                    """
                    SELECT ?autosampler_manufacturer ?laboratory ?autosampler_power_supply
                    ?site ?loc ?vial ?fill_level ?fill_level_om_value ?fill_level_unit ?fill_level_num_val
                    ?max_level ?max_level_om_value ?max_level_unit ?max_level_num_val ?chemical_solution
                    WHERE {
                        <%s> <%s> ?autosampler_manufacturer; <%s> ?laboratory; <%s> ?autosampler_power_supply.
                        <%s> <%s> ?site.
                        ?site <%s> ?vial; <%s> ?loc.
                        OPTIONAL {
                            ?vial <%s> ?fill_level.
                            ?fill_level <%s> ?fill_level_om_value.
                            ?fill_level_om_value <%s> ?fill_level_unit; <%s> ?fill_level_num_val.
                        }
                        OPTIONAL {
                            ?vial <%s> ?max_level.
                            ?max_level <%s> ?max_level_om_value.
                            ?max_level_om_value <%s> ?max_level_unit; <%s> ?max_level_num_val.
                        }
                        ?vial <%s> ?chemical_solution.
                    }
                    """ % (
                        given_autosampler_iri, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY,
                        given_autosampler_iri, ONTOVAPOURTEC_HASSITE, ONTOVAPOURTEC_HOLDS, ONTOVAPOURTEC_LOCATIONID,
                        ONTOVAPOURTEC_HASFILLLEVEL, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                        ONTOVAPOURTEC_HASMAXLEVEL, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                        ONTOVAPOURTEC_ISFILLEDWITH
                    )
            response = self.performQuery(query)
            unique_autosampler_list = [given_autosampler_iri]

        list_autosampler = []
        for specific_autosampler in unique_autosampler_list:
            info_of_specific_autosampler = self.get_sublist_in_list_of_dict_matching_key_value(response, 'autosampler', specific_autosampler) if given_autosampler_iri is None else response

            unique_specific_autosampler_manufacturer_iri = self.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'autosampler_manufacturer')
            if len(unique_specific_autosampler_manufacturer_iri) > 1:
                raise Exception("Multiple dbpedia:manufacturer identified (<%s>) in one instance of OntoVapourtec:AutoSampler %s is currently NOT supported." % ('>, <'.join(unique_specific_autosampler_manufacturer_iri), specific_autosampler))
            elif len(unique_specific_autosampler_manufacturer_iri) < 1:
                raise Exception("No instance of dbpedia:manufacturer was identified given instance of OntoVapourtec:AutoSampler: %s" % (specific_autosampler))
            else:
                unique_specific_autosampler_manufacturer_iri = unique_specific_autosampler_manufacturer_iri[0]

            unique_specific_autosampler_laboratory_iri = self.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'laboratory')
            if len(unique_specific_autosampler_laboratory_iri) > 1:
                raise Exception("Multiple OntoLab:isContainedIn OntoLab:Laboratory identified (<%s>) for one instance of OntoVapourtec:AutoSampler %s." % ('>, <'.join(unique_specific_autosampler_laboratory_iri), specific_autosampler))
            elif len(unique_specific_autosampler_laboratory_iri) < 1:
                raise Exception("No instance of OntoLab:isContainedIn OntoLab:Laboratory was identified given instance of OntoVapourtec:AutoSampler: %s" % (specific_autosampler))
            else:
                unique_specific_autosampler_laboratory_iri = unique_specific_autosampler_laboratory_iri[0]

            unique_specific_autosampler_power_supply_iri = self.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'autosampler_power_supply')
            if len(unique_specific_autosampler_power_supply_iri) > 1:
                raise Exception("Multiple OntoLab:hasPowerSupply OntoLab:PowerSupply identified (<%s>) for one instance of OntoVapourtec:AutoSampler %s is currently NOT supported." % ('>, <'.join(unique_specific_autosampler_power_supply_iri), specific_autosampler))
            elif len(unique_specific_autosampler_power_supply_iri) < 1:
                raise Exception("No instance of OntoLab:hasPowerSupply OntoLab:PowerSupply was identified given instance of OntoVapourtec:AutoSampler: %s" % (specific_autosampler))
            else:
                unique_specific_autosampler_power_supply_iri = unique_specific_autosampler_power_supply_iri[0]

            logger.debug("The sublist of all information related to the specific instance of OntoVapourtec:AutoSampler <%s> in the knowledge graph (%s): %s" %
                (specific_autosampler, self.kg_client.getQueryEndpoint(), str(info_of_specific_autosampler)))

            unique_site_list = self.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'site')
            list_autosampler_site = []
            for specific_site in unique_site_list:
                info_of_specific_site = self.get_sublist_in_list_of_dict_matching_key_value(info_of_specific_autosampler, 'site', specific_site)[0] # TODO here we assume only one, but to be varified by checking length
                # TODO add exceptions for lenth of the results, e.g. if len(info_of_specific_site) > 1:
                # TODO add logger info/debug
                referred_instance_of_ontocape_material = self.get_ontocape_material(info_of_specific_site['chemical_solution'], ONTOCAPE_REFERSTOMATERIAL)[0] # TODO here we assume only one, but to be varified by checking length
                # TODO add exceptions for lenth of the results, e.g. if len(referred_instance_of_ontocape_material) > 1:
                vial = Vial(
                    instance_iri=info_of_specific_site['vial'],
                    isFilledWith=ChemicalSolution(
                        instance_iri=info_of_specific_site['chemical_solution'],
                        refersToMaterial=referred_instance_of_ontocape_material,
                        fills=info_of_specific_site['vial'],
                        isPreparedBy=None # TODO add support for isPreparedBy
                    ),
                    hasFillLevel=OM_Volume(
                        instance_iri=info_of_specific_site['fill_level'],
                        hasValue=OM_Measure(
                            instance_iri=info_of_specific_site['fill_level_om_value'],
                            hasUnit=info_of_specific_site['fill_level_unit'],
                            hasNumericalValue=info_of_specific_site['fill_level_num_val']
                        )
                    ),
                    hasMaxLevel=OM_Volume(
                        instance_iri=info_of_specific_site['max_level'],
                        hasValue=OM_Measure(
                            instance_iri=info_of_specific_site['max_level_om_value'],
                            hasUnit=info_of_specific_site['max_level_unit'],
                            hasNumericalValue=info_of_specific_site['max_level_num_val']
                        )
                    ),
                    isHeldIn=specific_site
                )
                autosampler_site = AutoSamplerSite(
                    instance_iri=specific_site,
                    holds=vial,
                    locationID=info_of_specific_site['loc']
                )
                list_autosampler_site.append(autosampler_site)

            autosampler = AutoSampler(
                instance_iri=specific_autosampler,
                manufacturer=unique_specific_autosampler_manufacturer_iri,
                isContainedIn=unique_specific_autosampler_laboratory_iri,
                hasPowerSupply=unique_specific_autosampler_power_supply_iri,
                hasSite=list_autosampler_site
            )
            list_autosampler.append(autosampler)

        return list_autosampler

    def getExpReactionCondition(self, rxnexp_iri: str) -> List[ReactionCondition]:
        """
            This method retrieves a list of ReactionCondition pointed by the given instance of OntoRxn:ReactionExperiment/ReactionVariation.

            Arguments:
                rxnexp_iri - IRI of instance of OntoRxn:ReactionExperiment/ReactionVariation
        """

        # Delete "<" and ">" around the IRI
        rxnexp_iri = trimIRI(rxnexp_iri)

        # Construct query string
        query = PREFIX_RDFS + \
                PREFIX_RDF + \
                PREFIX_XSD + \
                PREFIX_OWL + \
                """SELECT DISTINCT ?condition ?clz ?measure ?val ?unit ?o ?id ?multi ?usage \
                WHERE { \
                ?subo rdfs:subPropertyOf* <%s> . \
                <%s> ?subo ?condition . \
                ?condition <%s> ?measure . \
                ?measure <%s> ?unit; \
                        <%s> ?val . \
                <%s> ?o ?condition . \
                ?condition rdf:type ?clz . \
                FILTER(?clz != owl:Thing && ?clz != owl:NamedIndividual && ?clz != <%s>) . \
                OPTIONAL {?condition <%s> ?id .} \
                OPTIONAL {?condition <%s> ?multi .} \
                OPTIONAL {?condition <%s> ?usage .} \
                }""" % (ONTORXN_HASREACTIONCONDITION, rxnexp_iri, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE, 
                rxnexp_iri, ONTORXN_REACTIONCONDITION, ONTODOE_POSITIONALID, ONTORXN_INDICATESMULTIPLICITYOF, ONTORXN_INDICATESUSAGEOF)

        # Perform SPARQL query
        response = self.performQuery(query)

        # Populate the list of ReactionCondition based on query results
        list_con = []
        for res in response:
            rxn_condition = ReactionCondition(
                instance_iri=res['condition'],
                clz=res['clz'],
                objPropWithExp=res['o'] if isinstance(res['o'], list) else [res['o']],
                hasValue=OM_Measure(instance_iri=res['measure'],hasUnit=res['unit'],hasNumericalValue=res['val']),
                positionalID=res['id'] if 'id' in res else None,
                indicatesMultiplicityOf=res['multi'] if 'multi' in res else None,
                indicateUsageOf=res['usage'] if 'usage' in res else None
            )
            list_con.append(rxn_condition)
        
        return list_con

    def getExpPerformanceIndicator(self, rxnexp_iri: str) -> List[PerformanceIndicator]:
        """
            This method retrieves a list of PerformanceIndicator pointed by the given instance of OntoRxn:ReactionExperiment/ReactionVariation.

            Arguments:
                rxnexp_iri - IRI of instance of OntoRxn:ReactionExperiment/ReactionVariation
        """

        # Delete "<" and ">" around the IRI
        rxnexp_iri = trimIRI(rxnexp_iri)

        # Construct query string
        query = PREFIX_RDFS + \
                PREFIX_RDF + \
                PREFIX_XSD + \
                PREFIX_OWL + \
                """SELECT DISTINCT ?perf ?clz ?measure ?val ?unit ?o ?id \
                WHERE { \
                ?subo rdfs:subPropertyOf* <%s> . \
                <%s> ?subo ?perf . \
                ?perf <%s> ?measure . \
                ?measure <%s> ?unit; \
                        <%s> ?val . \
                <%s> ?o ?perf . \
                ?perf rdf:type ?clz . \
                FILTER(?clz != owl:Thing && ?clz != owl:NamedIndividual && ?clz != <%s>) . \
                OPTIONAL {?perf <%s> ?id .} \
                }""" % (ONTORXN_HASPERFORMANCEINDICATOR, rxnexp_iri, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE, 
                rxnexp_iri, ONTORXN_PERFORMANCEINDICATOR, ONTODOE_POSITIONALID)
        
        # Perform SPARQL query
        response = self.performQuery(query)

        # Populate the list of PerformanceIndicator based on query results
        list_perf = []
        for res in response:
            perf_indicator = PerformanceIndicator(
                instance_iri=res['perf'],
                clz=res['clz'],
                objPropWithExp=res['o'] if isinstance(res['o'], list) else [res['o']],
                hasValue=OM_Measure(instance_iri=res['measure'],hasUnit=res['unit'],hasNumericalValue=res['val']),
                positionalID=res['id'] if 'id' in res else None
            )
            list_perf.append(perf_indicator)
        
        return list_perf

    def getDoEStrategy(self, strategy_iri: str) -> Strategy:
        """
            This method retrieves information given instance of dataclass OntoDoE.Strategy.

            Arguments:
                strategy_iri - iri of OntoDoE:Strategy instance
        """
        # Delete "<" and ">" around the IRI
        strategy_iri = trimIRI(strategy_iri)

        # Construct query string
        query = PREFIX_RDF + \
                PREFIX_OWL + \
                """SELECT ?strategy \
                WHERE { \
                <%s> rdf:type ?strategy . \
                FILTER(?strategy != owl:Thing && ?strategy != owl:NamedIndividual) . \
                }""" % (strategy_iri)
        
        # Perform SPARQL query
        response = self.performQuery(query)

        # Extract from the response
        if (len(response) > 1):
            raise Exception("Strategy Instance <%s> should only have one rdf:type." % (strategy_iri))
        res = [list(r.values())[0] for r in response]

        if (getShortName(res[0]) == TSEMO.__name__):
            tsemo_instance = self.getTSEMOSettings(strategy_iri)
            return tsemo_instance
        elif (getShortName(res[0]) == LHS.__name__):
            # TODO implement handling for LHS
            raise Exception("LHS as a OntoDoE:Strategy is not yet supported.")
        else:
            # TODO implement handling for other DoE strategy
            raise Exception("<%s> as a OntoDoE:Strategy is not yet supported." % (getShortName(res[0])))

    def getTSEMOSettings(self, tsemo_iri: str) -> TSEMO:
        """
            This method retrieves the settings of TSEMO algorithm as part of `Summit` package from the given instance of OntoDoE.TSEMO.
            (For TSEMO algorithm in package `Summit`, please visit: https://gosummit.readthedocs.io/en/latest/strategies.html#tsemo)

            Arguments:
                tsemo_iri - iri of OntoDoE:TSEMO instance
        """

        # Construct query string
        query = """SELECT ?nGenerations ?nRetries ?nSpectralPoints ?populationSize \
                WHERE { \
                OPTIONAL {
                    <%s> <%s> ?nGenerations . \
                    <%s> <%s> ?nRetries . \
                    <%s> <%s> ?nSpectralPoints . \
                    <%s> <%s> ?populationSize . \
                } \
                }""" % (tsemo_iri, ONTODOE_NGENERATIONS, tsemo_iri, ONTODOE_NRETRIES, tsemo_iri, ONTODOE_NSPECTRALPOINTS, tsemo_iri, ONTODOE_POPULATIONSIZE)

        # Perform SPARQL query
        response = self.performQuery(query)
        if (len(response) > 1):
            raise Exception("Instance <%s> should only have one set of settings." % (tsemo_iri))
        tsemo_instance = TSEMO(instance_iri=tsemo_iri,**response[0])
        return tsemo_instance

    def getNewExperimentFromDoE(self, doe_iri: str) -> str:
        doe_iri = trimIRI(doe_iri)

        query = """SELECT ?newexp \
                WHERE { <%s> <%s> ?newexp .}""" % (doe_iri, ONTODOE_PROPOSESNEWEXPERIMENT)

        response = self.performQuery(query)

        if (len(response) > 1):
            raise Exception(
                "DesignOfExperiment instance <%s> should only propose ONE instance of ReactionExperiment, it is currently proposing: <%s>" % (
                    doe_iri, ">, <".join([res['newexp'] for res in response])
                )
            )
        else:
            return response[0]['newexp']

    def create_equip_settings_from_rxn_exp(self, rxnexp: ReactionExperiment) -> List[EquipmentSettings]:
        list_equip_setting = []
        reactor_setting = ReactorSettings(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            hasResidenceTimeSetting=ResidenceTimeSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=self.get_rxn_con_or_perf_ind(rxnexp.hasReactionCondition, ONTORXN_RESIDENCETIME).instance_iri,
                namespace_for_init=getNameSpace(rxnexp.instance_iri)
            ),
            hasReactorTemperatureSetting=ReactorTemperatureSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=self.get_rxn_con_or_perf_ind(rxnexp.hasReactionCondition, ONTORXN_REACTIONTEMPERATURE).instance_iri,
                namespace_for_init=getNameSpace(rxnexp.instance_iri)
            ),
            namespace_for_init=getNameSpace(rxnexp.instance_iri)
        )

        list_equip_setting.append(reactor_setting)

        # TODO add support for PumpSettings
        # pump_setting = PumpSettings()
        # list_equip_setting.append(pump_setting)

        return list_equip_setting

    def write_equip_settings_to_kg(self, equip_settings: List[EquipmentSettings]):
        filePath = f'{str(uuid.uuid4())}.xml'

        g = Graph()
        for es in equip_settings:
            g = es.create_instance_for_kg(g)
        g.serialize(filePath, format='xml')
        self.uploadOntology(filePath)
        # Delete generated XML file
        # os.remove(filePath)

    def get_rxn_con_or_perf_ind(self, list_: List[ReactionCondition] or List[PerformanceIndicator], clz, positionalID=None):
        var = []
        for l in list_:
            # TODO what if unknown positionalID when calling this function? in fact, does positionalID matter?
            logger.error(l.__dict__)
            if tuple((l.clz, l.positionalID)) == tuple((clz, positionalID)):
                var.append(l)
        if len(var) > 1:
            raise Exception("Only one appearance should be allowed for a ReactionCondition/PerformanceIndicator to be set as a ParameterSetting, found: <%s>." % '>, <'.join(var))
        elif len(var) < 1:
            raise Exception("Class <%s> was not found." % clz)
            # raise Exception(len(list_))
        else:
            logger.error("Identified quantity")
            logger.error(var[0])
            return var[0]

    # \item queries knowledge graph to locate the suitable digital twin:
    # does it has the suitable chemicals?
    # does the hardware's operation range covers the reaction condition?
    # how many pending configurations does it have? -> locate the most suitable hardware (can be expanded to check how long is each configuration, what's the temperature etc.)
    # here we probably don't consider its status yet, just assume the equipment is there and will work - we rely on Execution Agent to keep track of the status
    def get_preferred_r4_reactor(self, rxnexp: ReactionExperiment) -> str:
        # query if there's suitable hardware
        # first step: query if suitable chemicals given the experiment --> does the vial hold the chemicals that has the same thermodynamicBehaviour as the InputChemical of a reaction experiment
        list_autosampler = self.get_all_autosampler_with_fill()
        list_input_chemical = self.get_input_chemical_of_rxn_exp(rxnexp.instance_iri)
        list_input_chemical_single_phase = [input_chem.thermodynamicBehaviour for input_chem in list_input_chemical]
        applicable_reactor = {}
        for autosampler in list_autosampler:
            list_chemical_solution_mat_single_phase = [site.holds.isFilledWith.refersToMaterial.thermodynamicBehaviour for site in autosampler.hasSite]
            if all(item in list_chemical_solution_mat_single_phase for item in list_input_chemical_single_phase):
                # second step: query if the operation range covers the reaction condition
                # NOTE here we only consider the reaction temperature at the moment, support for checking more reaction conditions (e.g. reactor material, autosampler liquid level, etc.) can be added later on
                # get the reactor first, then get the temperature conditon (cached value), compare the value if in the range, if yes then carry out the next step (how many prior experiment), otherwise skip
                vapourtec_rs400 = self.get_vapourtec_rs400_given_autosampler(autosampler)
                list_reactor = [lab_equip for lab_equip in vapourtec_rs400.consistsOf if isinstance(lab_equip, VapourtecR4Reactor)]

                temp_condition = [cond.hasValue.hasNumericalValue for cond in rxnexp.hasReactionCondition if cond.clz == ONTORXN_REACTIONTEMPERATURE]
                if len(temp_condition) > 1:
                    raise Exception()
                elif len(temp_condition) < 1:
                    raise Exception()
                else:
                    temp_condition = temp_condition[0]

                for reactor in list_reactor:
                    if reactor.hasReactorTemperatureLowerLimit.hasValue.hasNumericalValue <= temp_condition <= reactor.hasReactorTemperatureUpperLimit.hasValue.hasNumericalValue:
                        # currently the simplified version: check how many reaction is pending in front
                        num_pending_rxn = len(self.get_rxn_exp_pending_for_r4_reactor(reactor.instance_iri))
                        applicable_reactor[reactor.instance_iri] = num_pending_rxn
                        # TODO currently this function creats a dict that records the number of pending reactions for each VapourtecR4Reactor
                        # TODO future work should support recording of estimated time for the VapourtecR4Reactor to be available
                        # TODO maybe calculate based on residence time and other information {'labequip_1': 1, 'labequip_2': 2}

        # return the iri of the preferred r4 reactor
        return min(applicable_reactor, key=applicable_reactor.get)

    def get_autosampler(self, autosampler_iri: str) -> AutoSampler:
        autosampler = self.get_all_autosampler_with_fill(given_autosampler_iri=autosampler_iri)
        if len(autosampler) == 1:
            return autosampler[0]
        else:
            raise Exception("AutoSampler <%s> is not uniquely identified in the knowledge graph, retrieved results: %s" % (autosampler_iri, str(autosampler)))

    def get_vapourtec_rs400_given_autosampler(self, autosampler: AutoSampler) -> VapourtecRS400:
        query = PREFIX_RDF + \
                """
                SELECT ?rs400 ?rs400_manufacturer ?laboratory ?rs400_power_supply
                WHERE { ?rs400 <%s> <%s>; rdf:type <%s>; <%s> ?rs400_manufacturer; <%s> ?laboratory; <%s> ?rs400_power_supply. }
                """ % (
                    SAREF_CONSISTSOF, autosampler.instance_iri,
                    ONTOVAPOURTEC_VAPOURTECRS400, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY
                )

        response = self.performQuery(query)

        # NOTE here we are assuming one VapourtecRS400 module has only ONE manufacturer, locates in only ONE laboratory, also has only ONE type of power supply
        # NOTE this might not hold universally, but we will simplify for the moment
        if len(response) > 1:
            raise Exception("One AutoSampler (%s) should only be attached to one VapourtecRS400 module. Identified multiple: %s" % (autosampler.instance_iri, str(response)))
        elif len(response) < 1:
            raise Exception("The given AutoSampler (%s) is not associated with any instances of VapourtecRS400 module." % (autosampler.instance_iri))
        else:
            res = response[0]

        list_vapourtec_reactor_and_pump = []
        list_vapourtec_reactor_and_pump.append(autosampler)
        list_vapourtec_reactor_and_pump += self.get_r4_reactor_given_vapourtec_rs400(res['rs400'])
        list_vapourtec_reactor_and_pump += self.get_r2_pump_given_vapourtec_rs400(res['rs400'])

        vapourtec_rs400 = VapourtecRS400(
            instance_iri=res['rs400'],
            manufacturer=res['rs400_manufacturer'],
            isContainedIn=res['laboratory'],
            hasPowerSupply=res['rs400_power_supply'],
            consistsOf=list_vapourtec_reactor_and_pump
        )

        return vapourtec_rs400

    def get_rxn_exp_conducted_in_r4_reactor(self, r4_reactor_iri: str) -> List[str]:
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        query = """
                SELECT ?rxnexp WHERE {<%s> <%s> ?rxnexp.}
                """ % (r4_reactor_iri, ONTOVAPOURTEC_CONDUCTED)
        response = self.performQuery(query)
        list_rxn = [res['rxnexp'] for res in response]
        return list_rxn

    def get_rxn_exp_assigned_to_r4_reactor(self, r4_reactor_iri: str) -> List[str]:
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        query = """
                SELECT ?rxnexp WHERE {?rxnexp <%s> <%s>.}
                """ % (ONTORXN_ISASSIGNEDTO, r4_reactor_iri)
        response = self.performQuery(query)
        list_rxn = [res['rxnexp'] for res in response]
        return list_rxn

    def get_rxn_exp_pending_for_r4_reactor(self, r4_reactor_iri: str) -> List[str]:
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        query = """SELECT ?rxnexp WHERE { ?rxnexp <%s> <%s>. FILTER NOT EXISTS { <%s> <%s> ?rxnexp. } }""" % (
            ONTORXN_ISASSIGNEDTO, r4_reactor_iri, r4_reactor_iri, ONTOVAPOURTEC_CONDUCTED)
        response = self.performQuery(query)
        list_rxn = [res['rxnexp'] for res in response]
        return list_rxn

    def get_r4_reactor_rxn_exp_assigned_to(self, rxn_exp_iri: str) -> str:
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        query = """SELECT ?r4_reactor WHERE { <%s> <%s> ?r4_reactor. }""" % (rxn_exp_iri, ONTORXN_ISASSIGNEDTO)
        response = self.performQuery(query)
        r4_reactor_iri = [res['r4_reactor'] for res in response]
        if len(r4_reactor_iri) > 1:
            raise Exception("ReactionExperiment <%s> is assigned to multiple VapourtecR4Reactor: <%s>" % (rxn_exp_iri, '>, <'.join(r4_reactor_iri)))
        return r4_reactor_iri[0] if len(r4_reactor_iri) == 1 else None

    def get_r4_reactor_given_vapourtec_rs400(self, vapourtec_rs400_iri: str) -> List[VapourtecR4Reactor]:
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        query = PREFIX_RDF + \
                """
                SELECT ?r4_reactor ?r4_reactor_manufacturer ?laboratory ?r4_reactor_power_supply ?loc ?r4_reactor_material
                ?r4_reactor_internal_diameter ?r4_reactor_internal_diameter_measure ?r4_reactor_internal_diameter_unit ?r4_reactor_internal_diameter_val
                ?r4_reactor_length ?r4_reactor_length_measure ?r4_reactor_length_unit ?r4_reactor_length_val
                ?r4_reactor_volume ?r4_reactor_volume_measure ?r4_reactor_volume_unit ?r4_reactor_volume_val
                ?r4_reactor_temp_lower ?r4_reactor_temp_lower_measure ?r4_reactor_temp_lower_unit ?r4_reactor_temp_lower_val
                ?r4_reactor_temp_upper ?r4_reactor_temp_upper_measure ?r4_reactor_temp_upper_unit ?r4_reactor_temp_upper_val
                WHERE {
                    <%s> <%s> ?r4_reactor .
                    ?r4_reactor rdf:type <%s>; <%s> ?r4_reactor_manufacturer; <%s> ?laboratory; <%s> ?r4_reactor_power_supply; <%s> ?loc;
                    <%s> ?r4_reactor_material; <%s> ?r4_reactor_internal_diameter; <%s> ?r4_reactor_length;
                    <%s> ?r4_reactor_volume; <%s> ?r4_reactor_temp_lower; <%s> ?r4_reactor_temp_upper.
                    ?r4_reactor_internal_diameter <%s> ?r4_reactor_internal_diameter_measure. ?r4_reactor_internal_diameter_measure <%s> ?r4_reactor_internal_diameter_unit; <%s> ?r4_reactor_internal_diameter_val.
                    ?r4_reactor_length <%s> ?r4_reactor_length_measure. ?r4_reactor_length_measure <%s> ?r4_reactor_length_unit; <%s> ?r4_reactor_length_val.
                    ?r4_reactor_volume <%s> ?r4_reactor_volume_measure. ?r4_reactor_volume_measure <%s> ?r4_reactor_volume_unit; <%s> ?r4_reactor_volume_val.
                    ?r4_reactor_temp_lower <%s> ?r4_reactor_temp_lower_measure. ?r4_reactor_temp_lower_measure <%s> ?r4_reactor_temp_lower_unit; <%s> ?r4_reactor_temp_lower_val.
                    ?r4_reactor_temp_upper <%s> ?r4_reactor_temp_upper_measure. ?r4_reactor_temp_upper_measure <%s> ?r4_reactor_temp_upper_unit; <%s> ?r4_reactor_temp_upper_val.
                }
                """ % (
                    vapourtec_rs400_iri, SAREF_CONSISTSOF, ONTOVAPOURTEC_VAPOURTECR4REACTOR, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY, ONTOVAPOURTEC_LOCATIONID,
                    ONTOVAPOURTEC_HASREACTORMATERIAL, ONTOVAPOURTEC_HASINTERNALDIAMETER, ONTOVAPOURTEC_HASREACTORLENGTH,
                    ONTOVAPOURTEC_HASREACTORVOLUME, ONTOVAPOURTEC_HASREACTORTEMPERATURELOWERLIMIT, ONTOVAPOURTEC_HASREACTORTEMPERATUREUPPERLIMIT,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE
                )

        response = self.performQuery(query)
        unique_r4_reactor_list = self.get_unique_values_in_list_of_dict(response, 'r4_reactor')
        logger.debug("The list of all OntoVapourtec:VapourtecR4Reactor associated with the given instance of OntoVapourtec:VapourtecRS400 <%s>: %s" % (
            vapourtec_rs400_iri, str(unique_r4_reactor_list)
        ))
        list_r4_reactor = []
        for specific_r4_reactor in unique_r4_reactor_list:
            info_of_specific_r4_reactor = self.get_sublist_in_list_of_dict_matching_key_value(response, 'r4_reactor', specific_r4_reactor)
            if len(info_of_specific_r4_reactor) > 1:
                raise Exception("Multiple records of OntoVapourtec:VapourtecR4Reactor <%s> are identified: %s" % (specific_r4_reactor, str(info_of_specific_r4_reactor)))
            elif len(info_of_specific_r4_reactor) < 1:
                raise Exception("No record of OntoVapourtec:VapourtecR4Reactor <%s> are identified." % (specific_r4_reactor))
            else:
                info_ = info_of_specific_r4_reactor[0]

            r4_reactor = VapourtecR4Reactor(
                instance_iri=info_['r4_reactor'],
                manufacturer=info_['r4_reactor_manufacturer'],
                isContainedIn=info_['laboratory'],
                hasPowerSupply=info_['r4_reactor_power_supply'],
                locationID=info_['loc'],
                hasReactorMaterial=info_['r4_reactor_material'],
                hasInternalDiameter=OM_Diameter(
                    instance_iri=info_['r4_reactor_internal_diameter'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_internal_diameter_measure'],hasUnit=info_['r4_reactor_internal_diameter_unit'],hasNumericalValue=info_['r4_reactor_internal_diameter_val'])
                ),
                hasReactorLength=OM_Length(
                    instance_iri=info_['r4_reactor_length'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_length_measure'],hasUnit=info_['r4_reactor_length_unit'],hasNumericalValue=info_['r4_reactor_length_val'])
                ),
                hasReactorVolume=OM_Volume(
                    instance_iri=info_['r4_reactor_volume'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_volume_measure'],hasUnit=info_['r4_reactor_volume_unit'],hasNumericalValue=info_['r4_reactor_volume_val'])
                ),
                hasReactorTemperatureLowerLimit=OM_CelsiusTemperature(
                    instance_iri=info_['r4_reactor_temp_lower'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_temp_lower_measure'],hasUnit=info_['r4_reactor_temp_lower_unit'],hasNumericalValue=info_['r4_reactor_temp_lower_val'])
                ),
                hasReactorTemperatureUpperLimit=OM_CelsiusTemperature(
                    instance_iri=info_['r4_reactor_temp_upper'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_temp_upper_measure'],hasUnit=info_['r4_reactor_temp_upper_unit'],hasNumericalValue=info_['r4_reactor_temp_upper_val'])
                ),
                conducted=self.get_rxn_exp_conducted_in_r4_reactor(info_['r4_reactor'])
            )
            list_r4_reactor.append(r4_reactor)

        return list_r4_reactor

    def get_r2_pump_given_vapourtec_rs400(self, vapourtec_rs400_iri: str) -> List[VapourtecR2Pump]:
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        query = PREFIX_RDF + \
                """
                SELECT ?r2_pump ?r2_pump_manufacturer ?laboratory ?r2_pump_power_supply ?loc
                WHERE { <%s> <%s> ?r2_pump . ?r2_pump rdf:type <%s>; <%s> ?r2_pump_manufacturer; <%s> ?laboratory; <%s> ?r2_pump_power_supply; <%s> ?loc. }
                """ % (vapourtec_rs400_iri, SAREF_CONSISTSOF, ONTOVAPOURTEC_VAPOURTECR2PUMP, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY, ONTOVAPOURTEC_LOCATIONID)

        response = self.performQuery(query)
        unique_r2_pump_list = self.get_unique_values_in_list_of_dict(response, 'r2_pump')
        logger.debug("The list of all OntoVapourtec:VapourtecR2Pump associated with the given instance of OntoVapourtec:VapourtecRS400 <%s>: %s" % (
            vapourtec_rs400_iri, str(unique_r2_pump_list)
        ))
        list_r2_pump = []
        for specific_r2_pump in unique_r2_pump_list:
            info_of_specific_r2_pump = self.get_sublist_in_list_of_dict_matching_key_value(response, 'r2_pump', specific_r2_pump)

            if len(info_of_specific_r2_pump) > 1:
                raise Exception("Multiple records of OntoVapourtec:VapourtecR4Reactor <%s> are identified: %s" % (specific_r2_pump, str(info_of_specific_r2_pump)))
            elif len(info_of_specific_r2_pump) < 1:
                raise Exception("No record of OntoVapourtec:VapourtecR4Reactor <%s> are identified." % (specific_r2_pump))
            else:
                info_ = info_of_specific_r2_pump[0]

            r2_pump = VapourtecR2Pump(
                instance_iri=info_['r2_pump'],
                manufacturer=info_['r2_pump_manufacturer'],
                isContainedIn=info_['laboratory'],
                hasPowerSupply=info_['r2_pump_power_supply'],
                locationID=info_['loc']
            )
            list_r2_pump.append(r2_pump)

        return list_r2_pump

    def assign_rxn_exp_to_r4_reactor(self, rxn_exp_iri: str, r4_reactor_iri: str):
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        update = """INSERT DATA {<%s> <%s> <%s>}""" % (rxn_exp_iri, ONTORXN_ISASSIGNEDTO, r4_reactor_iri)
        self.performUpdate(update)
        logger.info("ReactionExperiment <%s> is now assigned to VapourtecR4Reactor <%s>." % (rxn_exp_iri, r4_reactor_iri))

    def remove_rxn_exp_from_r4_reactor(self, rxn_exp_iri: str, r4_reactor_iri: str):
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        update = """DELETE DATA {<%s> <%s> <%s>}""" % (rxn_exp_iri, ONTORXN_ISASSIGNEDTO, r4_reactor_iri)
        self.performUpdate(update)
        logger.info("ReactionExperiment <%s> is no longer assigned to VapourtecR4Reactor <%s>." % (rxn_exp_iri, r4_reactor_iri))

    def get_sublist_in_list_of_dict_matching_key_value(self, list_of_dict: List[Dict], key: str, value: Any) -> list:
        if len(list_of_dict) > 0:
            try:
                sublist = [d for d in list_of_dict if d[key] == value]
            except KeyError:
                logger.error("Key '%s' is not found in the given list of dict: %s" % (key, str(list_of_dict)))
            else:
                return sublist
        else:
            logger.error("An empty list is passed in while requesting return sublist given key '%s'." % (key))
            return []

    def get_unique_values_in_list_of_dict(self, list_of_dict: List[dict], key: str) -> list:
        return list(set(self.get_value_from_list_of_dict(list_of_dict, key)))

    def get_value_from_list_of_dict(self, list_of_dict: List[dict], key: str) -> list:
        if len(list_of_dict) > 0:
            try:
                list_of_values = [d[key] for d in list_of_dict]
            except KeyError:
                logger.error("Key '%s' is not found in the given list of dict: %s" % (key, str(list_of_dict)))
                return []
            else:
                return list_of_values
        else:
            logger.error("An empty list is passed in while requesting return value of key '%s'." % (key))
            return []
