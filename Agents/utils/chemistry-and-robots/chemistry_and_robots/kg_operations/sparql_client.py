# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from lib2to3.pgen2.pgen import ParserGenerator
from typing import Tuple
from urllib import response
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
from chemistry_and_robots.hardware import hplc

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
                        isAssignedTo=self.get_r4_reactor_rxn_exp_assigned_to(exp_iri),
                        isOccurenceOf=self.get_chemical_reaction(exp_iri)
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
                        isAssignedTo=self.get_r4_reactor_rxn_exp_assigned_to(exp_iri),
                        isOccurenceOf=self.get_chemical_reaction(exp_iri)
                    )
                )
        return list_exp

    def get_chemical_reaction(self, rxnexp_iri: str) -> OntoCAPE_ChemicalReaction:
        rxnexp_iri = trimIRI(rxnexp_iri)
        query = PREFIX_RDF + """SELECT DISTINCT ?chem_rxn ?reactant ?reac_type ?reac_species ?product ?prod_type ?prod_species
                   ?catalyst ?cata_type ?cata_species ?solvent ?solv_type ?solv_species
                   WHERE {
                       VALUES ?reac_type {<%s> <%s>}.
                       VALUES ?prod_type {<%s> <%s> <%s> <%s>}.
                       <%s> <%s> ?chem_rxn.
                       ?chem_rxn <%s> ?reactant; <%s> ?product.
                       ?reactant rdf:type ?reac_type; <%s> ?reac_species.
                       ?product rdf:type ?prod_type; <%s> ?prod_species.
                       optional{VALUES ?cata_type {<%s> <%s>}. ?chem_rxn <%s> ?catalyst. ?catalyst rdf:type ?cata_type; <%s> ?cata_species.}
                       optional{VALUES ?solv_type {<%s> <%s>}. ?chem_rxn <%s> ?solvent. ?solvent rdf:type ?solv_type; <%s> ?solv_species.}
                   }""" % (ONTOKIN_SPECIES, ONTOKIN_REACTANT, ONTOKIN_SPECIES, ONTOKIN_PRODUCT, ONTORXN_TARGETPRODUCT, ONTORXN_IMPURITY,
                   rxnexp_iri, ONTORXN_ISOCCURENCEOF, ONTOCAPE_HASREACTANT, ONTOCAPE_HASPRODUCT,
                   ONTOSPECIES_HASUNIQUESPECIES, ONTOSPECIES_HASUNIQUESPECIES,
                   ONTOKIN_SPECIES, ONTORXN_CATALYST, ONTOCAPE_CATALYST, ONTOSPECIES_HASUNIQUESPECIES,
                   ONTOKIN_SPECIES, ONTORXN_SOLVENT, ONTORXN_HASSOLVENT, ONTOSPECIES_HASUNIQUESPECIES)
        response = self.performQuery(query)
        logger.debug(response)

        unique_chem_rxn = self.get_unique_values_in_list_of_dict(response, 'chem_rxn')
        if len(unique_chem_rxn) > 1:
            raise Exception("Multiple OntoCAPE:ChemicalReaction identified for reaction experiment <%s>: %s" % (rxnexp_iri, str(unique_chem_rxn)))
        elif len(unique_chem_rxn) < 1:
            raise Exception("Reaction experiment <%s> is missing OntoCAPE:ChemicalReaction." % (rxnexp_iri))
        else:
            unique_chem_rxn = unique_chem_rxn[0]

        chem_rxn = OntoCAPE_ChemicalReaction(
            instance_iri=unique_chem_rxn,
            hasReactant=self.get_ontokin_species_from_chem_rxn(response, 'reactant', 'reac_type', 'reac_species'),
            hasProduct=self.get_ontokin_species_from_chem_rxn(response, 'product', 'prod_type', 'prod_species'),
            hasCatalyst=self.get_ontokin_species_from_chem_rxn(response, 'catalyst', 'cata_type', 'cata_species'),
            hasSolvent=self.get_ontokin_species_from_chem_rxn(response, 'solvent', 'solv_type', 'solv_species')
        )

        return chem_rxn

    def get_ontokin_species_from_chem_rxn(self, list_of_dict: List[Dict], species_role: str, species_type: str, ontospecies_key: str) -> List[OntoKin_Species]:
        """Example: species_role='reactant', species_type='reac_type', species_key='reac_species'"""
        list_species = []
        if not self.check_if_key_in_list_of_dict(list_of_dict, species_role):
            return None
        else:
            wanted_info = self.keep_wanted_keys_from_list_of_dict(list_of_dict, [species_role, species_type, ontospecies_key])
            wanted_info = self.remove_duplicate_dict_from_list_of_dict(wanted_info)
            unique_species_iri = self.get_unique_values_in_list_of_dict(wanted_info, species_role)
            for u_s in unique_species_iri:
                u_s_info = self.get_sublist_in_list_of_dict_matching_key_value(wanted_info, species_role, u_s)
                if len(u_s_info) > 1:
                    raise Exception("Multiple OntoSpecies:Species identified given one instance of %s <%s>: %s" % (species_role ,u_s, str(u_s_info)))
                elif len(u_s_info) < 1:
                    raise Exception("No unique OntoSpecies:Species identified for instance of %s <%s>." % (species_role, u_s))
                else:
                    u_s_info = u_s_info[0]
                if u_s_info[species_type] == ONTOKIN_SPECIES:
                    species = OntoKin_Species(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOKIN_REACTANT:
                    species = OntoKin_Reactant(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOKIN_PRODUCT:
                    species = OntoKin_Product(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTORXN_TARGETPRODUCT:
                    species = TargetProduct(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTORXN_IMPURITY:
                    species = Impurity(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTORXN_CATALYST:
                    species = Catalyst(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTORXN_SOLVENT:
                    species = Solvent(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                else:
                    raise Exception("Species type (%s) NOT supported for: %s" % (u_s_info[species_type], str(u_s_info)))
                list_species.append(species)

            return list_species

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

    def get_ontocape_material(self, subject_iri, predicate_iri, desired_type: str=None) -> List[OntoCAPE_Material]:
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
                            raise NotImplementedError("Support for <%s> as OntoCAPE_PhaseComponentConcentration is NOT implemented yet." % r['concentration_type'])
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

                if desired_type is None:
                    if predicate_iri == ONTORXN_HASINPUTCHEMICAL:
                        ocm_instance = InputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                    elif predicate_iri == ONTORXN_HASOUTPUTCHEMICAL:
                        ocm_instance = OutputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                    else:
                        ocm_instance = OntoCAPE_Material(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                elif desired_type == ONTORXN_INPUTCHEMICAL:
                    ocm_instance = InputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                elif desired_type == ONTORXN_OUTPUTCHEMICAL:
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

    def create_equip_settings_for_rs400_from_rxn_exp(self, rxnexp: ReactionExperiment, rs400: VapourtecRS400, preferred_r4_reactor: VapourtecR4Reactor) -> List[EquipmentSettings]:
        list_equip_setting = []
        reactor_setting = ReactorSettings(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            hasResidenceTimeSetting=ResidenceTimeSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=self.get_rxn_con_or_perf_ind(rxnexp.hasReactionCondition, ONTORXN_RESIDENCETIME),
                namespace_for_init=getNameSpace(rxnexp.instance_iri)
            ),
            hasReactorTemperatureSetting=ReactorTemperatureSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=self.get_rxn_con_or_perf_ind(rxnexp.hasReactionCondition, ONTORXN_REACTIONTEMPERATURE),
                namespace_for_init=getNameSpace(rxnexp.instance_iri)
            ),
            namespace_for_init=getNameSpace(rxnexp.instance_iri),
            specifies=preferred_r4_reactor,
            wasGeneratedFor=rxnexp.instance_iri
        )

        list_equip_setting.append(reactor_setting)

        # TODO add support for PumpSettings
        # first identify the reference reactant
        dict_stoi_ratio = {cond.indicatesMultiplicityOf:cond for cond in rxnexp.hasReactionCondition if cond.clz == ONTORXN_STOICHIOMETRYRATIO}
        reference_input_chemical = [chem for chem in dict_stoi_ratio if abs(dict_stoi_ratio.get(chem).hasValue.hasNumericalValue - 1) < 0.000001]
        if len(reference_input_chemical) > 1:
            raise Exception("Multiple reference chemicals are identified: " + str(reference_input_chemical) + " for reaction experiment: " + str(rxnexp.dict()))
        elif len(reference_input_chemical) < 1:
            raise Exception("No reference chemicals are identified for reaction experiment: " + str(rxnexp.dict()))
        else:
            reference_input_chemical = reference_input_chemical[0]

        reaction_scale = [cond for cond in rxnexp.hasReactionCondition if cond.clz == ONTORXN_REACTIONSCALE]
        if len(reaction_scale) > 1:
            raise Exception("Multiple reaction scale are identified: " + str(reaction_scale) + " for reaction experiment: " + str(rxnexp.dict()))
        elif len(reaction_scale) < 1:
            raise Exception("No reaction scale are identified for reaction experiment: " + str(rxnexp.dict()))
        else:
            reaction_scale = reaction_scale[0]

        if reference_input_chemical != reaction_scale.indicateUsageOf:
            raise Exception("Reference input chemical is specified as <%s>, however, the reaction scale is spedified for <%s> in reaction experiment <%s>." % (
                reference_input_chemical, reaction_scale.indicateUsageOf, rxnexp.instance_iri))

        dict_pumps = self.sort_r2_pumps_in_vapourtec_rs400(rs400)
        reference_pump = dict_pumps.pop("A") # NOTE here we hardcode implicit knowledge that Pump A is the reference pump
        autosampler = self.get_autosampler_from_vapourtec_rs400(rs400)
        for input_chem in rxnexp.hasInputChemical:
            # first get the AutoSamplerSite that contains the suitable chemical
            autosamplersite = self.get_autosampler_site_given_input_chemical(autosampler, input_chem)

            # then check if it's the reference reactant, if so, assign the reference pump
            if input_chem.instance_iri == reference_input_chemical:
                pump_setting = PumpSettings(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(rxnexp.instance_iri),
                    specifies=reference_pump,
                    hasStoichiometryRatioSetting=StoichiometryRatioSetting(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(rxnexp.instance_iri),
                        hasQuantity=dict_stoi_ratio.get(input_chem.instance_iri)
                    ),
                    hasSampleLoopVolumeSetting=SampleLoopVolumeSetting( # this is specific setting that refers to the Pump A (reference pump)
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(rxnexp.instance_iri),
                        hasQuantity=reaction_scale
                    ),
                    pumpsLiquidFrom=autosamplersite,
                    wasGeneratedFor=rxnexp.instance_iri
                )
            else:
                key = list(dict_pumps.keys())[0]
                pump = dict_pumps.pop(key)
                pump_setting = PumpSettings(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(rxnexp.instance_iri),
                    specifies=pump,
                    hasStoichiometryRatioSetting=StoichiometryRatioSetting(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(rxnexp.instance_iri),
                        hasQuantity=dict_stoi_ratio.get(input_chem.instance_iri)
                    ),
                    pumpsLiquidFrom=autosamplersite,
                    wasGeneratedFor=rxnexp.instance_iri
                )
            list_equip_setting.append(pump_setting)

        return list_equip_setting

    def get_autosampler_site_given_input_chemical(self, autosampler: AutoSampler, input_chem: InputChemical) -> AutoSamplerSite:
        for site in autosampler.hasSite:
            if site.holds.isFilledWith.refersToMaterial.thermodynamicBehaviour == input_chem.thermodynamicBehaviour:
                return site
        return None

    def get_autosampler_from_vapourtec_rs400(self, rs400: VapourtecRS400) -> AutoSampler:
        # NOTE this method assumes there's only one AutoSampler associated with one VapourtecRS400 module
        for equip in rs400.consistsOf:
            if isinstance(equip, AutoSampler):
                return equip
        return None

    def sort_r2_pumps_in_vapourtec_rs400(self, rs400: VapourtecRS400) -> Dict[str, VapourtecR2Pump]:
        return {pump.locationID:pump for pump in rs400.consistsOf if isinstance(pump, VapourtecR2Pump)}

    def write_equip_settings_to_kg(self, equip_settings: List[EquipmentSettings]):
        filePath = f'{str(uuid.uuid4())}.ttl'

        g = Graph()
        for es in equip_settings:
            g = es.create_instance_for_kg(g)
        g.serialize(filePath, format='ttl')
        self.uploadOntology(filePath)
        # Delete generated TTL file
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

    def get_preferred_vapourtec_rs400(self, rxnexp: ReactionExperiment) -> Tuple[VapourtecRS400, VapourtecR4Reactor]:
        """ This function queries the digital twin of the most suitable VapourtecRS400 for the given reaction experiment."""
        # first step: query if suitable chemicals given the experiment --> does the vial hold the chemicals that has the same thermodynamicBehaviour as the InputChemical of a reaction experiment
        list_autosampler = self.get_all_autosampler_with_fill()
        list_input_chemical = self.get_input_chemical_of_rxn_exp(rxnexp.instance_iri)
        list_input_chemical_single_phase = [input_chem.thermodynamicBehaviour for input_chem in list_input_chemical]
        for autosampler in list_autosampler:
            list_chemical_solution_mat_single_phase = [site.holds.isFilledWith.refersToMaterial.thermodynamicBehaviour for site in autosampler.hasSite]
            logger.debug([sp.dict() for sp in list_chemical_solution_mat_single_phase])
            logger.debug([sp.dict() for sp in list_input_chemical_single_phase])
            if all(item in list_chemical_solution_mat_single_phase for item in list_input_chemical_single_phase):
                # second step: query if the operation range covers the reaction condition
                # NOTE here we only consider the reaction temperature at the moment, support for checking more reaction conditions (e.g. reactor material, autosampler liquid level, etc.) can be added later on
                # get the reactor first, then get the temperature conditon (cached value), compare the value if in the range, if yes then carry out the next step (how many prior experiment), otherwise skip
                vapourtec_rs400 = self.get_vapourtec_rs400_given_autosampler(autosampler)
                list_reactor = [lab_equip for lab_equip in vapourtec_rs400.consistsOf if isinstance(lab_equip, VapourtecR4Reactor)]

                temp_condition = [cond.hasValue.hasNumericalValue for cond in rxnexp.hasReactionCondition if cond.clz == ONTORXN_REACTIONTEMPERATURE]
                if len(temp_condition) > 1:
                    raise Exception("Multiple reaction temperature conditions are found in the given reaction experiment: %s" % rxnexp)
                elif len(temp_condition) < 1:
                    raise Exception("No reaction temperature conditions are found in the given reaction experiment: %s" % rxnexp)
                else:
                    temp_condition = temp_condition[0]

                for reactor in list_reactor:
                    # TODO NOTE here the temperature comparison need to be modified to support different temperature units
                    if reactor.hasReactorTemperatureLowerLimit.hasValue.hasNumericalValue <= temp_condition <= reactor.hasReactorTemperatureUpperLimit.hasValue.hasNumericalValue:
                        # here we also checks if the amount of InputChemical is less than the amount of VapourtecR2Pumps available
                        if len(list_input_chemical) <= len([pump_ for pump_ in vapourtec_rs400.consistsOf if isinstance(pump_, VapourtecR2Pump)]):
                            # return the first reactor that is idle
                            # TODO future work should support recording of estimated time for the VapourtecR4Reactor to be available (just in case None of the reactor is available)
                            # TODO maybe calculate based on residence time and other information {'labequip_1': 1, 'labequip_2': 2}
                            if vapourtec_rs400.hasState == ONTOVAPOURTEC_IDLE:
                                return vapourtec_rs400, reactor

        return None, None

    def update_vapourtec_rs400_state(self, vapourtec_rs400_iri: str, target_state: str):
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        target_state = trimIRI(target_state)
        if target_state in LIST_ONTOVAPOURTEC_VALIDSTATE:
            update = """DELETE {<%s> <%s> ?state} INSERT {<%s> <%s> <%s>} WHERE {<%s> <%s> ?state}""" % (
                vapourtec_rs400_iri, SAREF_HASSTATE, vapourtec_rs400_iri, SAREF_HASSTATE, target_state, vapourtec_rs400_iri, SAREF_HASSTATE
            )
            self.performUpdate(update)
        else:
            raise Exception("Target state <%s> is not recognised as a valid state for VapourtecRS400, the intended instance of VapourtecRS400 is <%s>." % (target_state, vapourtec_rs400_iri))

    def get_autosampler(self, autosampler_iri: str) -> AutoSampler:
        autosampler = self.get_all_autosampler_with_fill(given_autosampler_iri=autosampler_iri)
        if len(autosampler) == 1:
            return autosampler[0]
        else:
            raise Exception("AutoSampler <%s> is not uniquely identified in the knowledge graph, retrieved results: %s" % (autosampler_iri, str(autosampler)))

    def get_vapourtec_rs400_given_autosampler(self, autosampler: AutoSampler) -> VapourtecRS400:
        query = PREFIX_RDF + \
                """
                SELECT ?rs400 ?rs400_manufacturer ?laboratory ?rs400_power_supply ?state
                WHERE { ?rs400 <%s> <%s>; rdf:type <%s>; <%s> ?rs400_manufacturer; <%s> ?laboratory; <%s> ?rs400_power_supply; <%s> ?state. }
                """ % (
                    SAREF_CONSISTSOF, autosampler.instance_iri,
                    ONTOVAPOURTEC_VAPOURTECRS400, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY, SAREF_HASSTATE
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
            consistsOf=list_vapourtec_reactor_and_pump,
            hasState=res['state']
        )

        return vapourtec_rs400

    # TODO commented out for now, decide whether to keep it before merging back to develop
    # def get_rxn_exp_conducted_in_r4_reactor(self, r4_reactor_iri: str) -> List[str]:
    #     r4_reactor_iri = trimIRI(r4_reactor_iri)
    #     query = """
    #             SELECT ?rxnexp WHERE {<%s> <%s> ?rxnexp.}
    #             """ % (r4_reactor_iri, ONTOVAPOURTEC_CONDUCTED)
    #     response = self.performQuery(query)
    #     list_rxn = [res['rxnexp'] for res in response]
    #     return list_rxn

    def get_rxn_exp_assigned_to_r4_reactor(self, r4_reactor_iri: str) -> List[str]:
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        query = """
                SELECT ?rxnexp WHERE {?rxnexp <%s> <%s>.}
                """ % (ONTORXN_ISASSIGNEDTO, r4_reactor_iri)
        response = self.performQuery(query)
        list_rxn = [res['rxnexp'] for res in response]
        return list_rxn

    # TODO commented out for now, decide whether to keep it before merging back to develop
    # def get_rxn_exp_pending_for_r4_reactor(self, r4_reactor_iri: str) -> List[str]:
    #     r4_reactor_iri = trimIRI(r4_reactor_iri)
    #     query = """SELECT ?rxnexp WHERE { ?rxnexp <%s> <%s>. FILTER NOT EXISTS { <%s> <%s> ?rxnexp. } }""" % (
    #         ONTORXN_ISASSIGNEDTO, r4_reactor_iri, r4_reactor_iri, ONTOVAPOURTEC_CONDUCTED)
    #     response = self.performQuery(query)
    #     list_rxn = [res['rxnexp'] for res in response]
    #     return list_rxn

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
                # conducted=self.get_rxn_exp_conducted_in_r4_reactor(info_['r4_reactor']) # TODO commented out for now, decide whether to keep it before merging back to develop
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

    def get_prior_rxn_exp_in_queue(self, rxn_exp_iri: str) -> Dict[str, int]:
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        query = PREFIX_RDF + """
                SELECT ?rxn ?timestamp
                WHERE {
                    <%s> <%s> ?dd. ?dd <%s> <%s>; <%s>/<%s>/<%s> ?specific_timestamp.
                    VALUES ?type {<%s> <%s>}.
                    ?rxn rdf:type ?type; <%s> ?doe_derivation.
                    ?doe_derivation <%s> <%s>; <%s>/<%s>/<%s> ?timestamp.
                    ?rxn ^<%s> ?exe_derivation.
                    ?exe_derivation <%s> <%s>.
                    ?exe_derivation <%s>/rdf:type ?status_type.
                    filter(?timestamp < ?specific_timestamp)
                }
                """ % (rxn_exp_iri, ONTODERIVATION_BELONGSTO, ONTODERIVATION_ISDERIVEDUSING, DOEAGENT_SERVICE, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION,
                ONTORXN_REACTIONEXPERIMENT, ONTORXN_REACTIONVARIATION, ONTODERIVATION_BELONGSTO, ONTODERIVATION_ISDERIVEDUSING, DOEAGENT_SERVICE,
                TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION, ONTORXN_ISASSIGNEDTO, ONTODERIVATION_ISDERIVEDUSING, EXEAGENT_SERVICE, ONTODERIVATION_HASSTATUS)
        response = self.performQuery(query)
        rxn_exp_queue = {res['rxn']:res['timestamp'] for res in response}
        return rxn_exp_queue

    def get_hplc_local_report_folder_path(self, hplc_iri: str):
        hplc_iri = trimIRI(hplc_iri)
        query = """SELECT ?report_dir ?report_extension WHERE { <%s> <%s> ?report_dir; <%s> ?report_extension. }""" % (hplc_iri, ONTOHPLC_LOCALREPORTDIRECTORY, ONTOHPLC_REPORTEXTENSION)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple report folders found for given instance of HPLC <%s>: %s" % (hplc_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No report folders found for given instance of HPLC <%s>." % (hplc_iri))
        else:
            return response[0]['report_dir'], response[0]['report_extension']

    def write_hplc_report_path_to_kg(self, hplc_report_iri: str, remote_report_path: str, local_file_path: str, timestamp_last_modified: float, timestamp_upload: float):
        # TODO "%s"^^xsd:string needs to be changed to <%s> once the PR on returning the full URL of the uploaded file is completed
        update = PREFIX_XSD + """INSERT DATA {<%s> a <%s>; <%s> "%s"^^xsd:string; <%s> "%s"^^xsd:string; <%s> %f; <%s> %f.}""" % (
            hplc_report_iri, ONTOHPLC_HPLCREPORT, ONTOHPLC_HASREPORTPATH, remote_report_path, ONTOHPLC_LOCALREPORTFILE, local_file_path,
            ONTOHPLC_LASTLOCALMODIFIEDAT, timestamp_last_modified, ONTOHPLC_LASTUPLOADEDAT, timestamp_upload)
        self.performUpdate(update)

    def get_raw_hplc_report_path_and_extension(self, hplc_report_iri: str) -> Tuple[str, str]:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = """SELECT ?path ?extension WHERE {<%s> <%s> ?path . ?hplc <%s>/<%s> <%s> . ?hplc <%s> ?extension .}""" % (
            hplc_report_iri, ONTOHPLC_HASREPORTPATH, ONTOHPLC_HASJOB, ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_REPORTEXTENSION)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple matches for filepath and extension for raw HPLC report <%s> was found: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No raw HPLC report found for <%s>" % hplc_report_iri)
        else:
            res = response[0]
        return res['path'], res['extension']

    def get_matching_species_from_hplc_results(self, retention_time: RetentionTime, hplc_method: HPLCMethod) -> str:
        # TODO here we took a shortcut, but in theory unit should also be checked - unit conversion is a generic problem that needs to be solved...
        hplc_rt = {rt.refersToSpecies:rt.hasValue.hasNumericalValue for rt in hplc_method.hasRetentionTime}
        rt_diff = {key: abs(hplc_rt[key] - retention_time.hasValue.hasNumericalValue) for key in hplc_rt}
        key_min_rt_diff = min(rt_diff, key=rt_diff.get)
        if rt_diff.get(key_min_rt_diff) < hplc.RETENTION_TIME_MATCH_THRESHOLD:
            return key_min_rt_diff
        else:
            raise Exception("No OntoSpecies:Species identified for OntoHPLC:RetentionTime instance (%s) given RETENTION_TIME_MATCH_THRESHOLD of %s and OntoHPLC:HPLCMethod (%s)" % (
                retention_time, hplc.RETENTION_TIME_MATCH_THRESHOLD, hplc_method))

    def get_internal_standard(self, hplc_method_iri: str) -> InternalStandard:
        hplc_method_iri = trimIRI(hplc_method_iri)
        query = PREFIX_RDF+"""SELECT ?is ?species ?property ?property_type ?value ?unit ?num_val
                   WHERE {
                       VALUES ?type {<%s>}.
                       <%s> <%s> ?is.
                       ?is rdf:type ?type; <%s> ?species; <%s> ?property.
                       ?property rdf:type ?property_type; <%s> ?value.
                       ?value <%s> ?unit; <%s> ?num_val.
                   }""" % (ONTOHPLC_INTERNALSTANDARD, hplc_method_iri, ONTOHPLC_USESINTERNALSTANDARD,
                ONTOCAPE_REPRESENTSOCCURENCEOF, ONTOCAPE_HASPROPERTY, ONTOCAPE_HASVALUE, ONTOCAPE_HASUNITOFMEASURE, ONTOCAPE_NUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise NotImplementedError("Multiple instances of InternalStandard (%s) are identified for HPLCMethod <%s>, this is NOT support yet." % (
                str(response), hplc_method_iri))
        elif len(response) < 1:
            raise Exception("InternalStandard NOT found for HPLCMethod <%s>" % hplc_method_iri)
        else:
            r = response[0]

        if r['property_type'] == OntoCAPE_Molarity.__fields__['clz'].default:
            concentration = OntoCAPE_Molarity(instance_iri=r['property'],
                hasValue=OntoCAPE_ScalarValue(instance_iri=r['value'],numericalValue=r['num_val'],hasUnitOfMeasure=r['unit']))
        else:
            # TODO add support for other type of OntoCAPE_PhaseComponentConcentration
            raise NotImplementedError("Support for <%s> as OntoCAPE_PhaseComponentConcentration is NOT implemented yet." % r['property_type'])
        internal_standard = InternalStandard(
            instance_iri=r['is'],
            hasProperty=concentration,
            representsOccurenceOf=r['species']
        )
        return internal_standard

    def get_hplc_method_given_hplc_report(self, hplc_report_iri: str) -> HPLCMethod:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = PREFIX_RDF+"""SELECT ?hplc_method WHERE {?hplc_job rdf:type <%s>; <%s> <%s>; <%s> ?hplc_method}""" % (
            ONTOHPLC_HPLCJOB, ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_USESMETHOD)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple instances identified for HPLCMethod given HPLCReport <%s>: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of HPLCMethod found given the HPLCReport <%s>" % hplc_report_iri)
        else:
            return self.get_hplc_method(response[0]['hplc_method'])

    def get_hplc_method(self, hplc_method_iri: str) -> HPLCMethod:
        hplc_method_iri = trimIRI(hplc_method_iri)
        query = PREFIX_RDF + """SELECT ?type ?r ?species ?measure ?unit ?value
                   WHERE {
                       VALUES ?type {<%s> <%s>}.
                       <%s> <%s>|<%s> ?r.
                       ?r rdf:type ?type; <%s> ?species; <%s> ?measure.
                       ?measure <%s> ?unit; <%s> ?value.
                   }
                """ % (ONTOHPLC_RESPONSEFACTOR, ONTOHPLC_RETENTIONTIME, hplc_method_iri, ONTOHPLC_HASRESPONSEFACTOR, ONTOHPLC_HASRETENTIONTIME,
                    ONTOHPLC_REFERSTOSPECIES, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)

        response = self.performQuery(query)

        list_rf = [] # list of ResponseFactor
        list_rt = [] # list of RetentionTime
        for res in response:
            if res['type'] == ONTOHPLC_RESPONSEFACTOR:
                rf = ResponseFactor(
                    instance_iri=res['r'],
                    hasValue=OM_Measure(
                        instance_iri=res['measure'],
                        hasUnit=res['unit'],
                        hasNumericalValue=res['value']
                    ),
                    refersToSpecies=res['species']
                )
                list_rf.append(rf)
            elif res['type'] == ONTOHPLC_RETENTIONTIME:
                rt = RetentionTime(
                    instance_iri=res['r'],
                    hasValue=OM_Measure(
                        instance_iri=res['measure'],
                        hasUnit=res['unit'],
                        hasNumericalValue=res['value']
                    ),
                    refersToSpecies=res['species']
                )
                list_rt.append(rt)

        comment = self.performQuery(PREFIX_RDFS+"SELECT ?comment WHERE {<%s> rdfs:comment ?comment}" % hplc_method_iri)[0]['comment']
        hplc_method = HPLCMethod(
            instance_iri=hplc_method_iri,
            hasResponseFactor=list_rf,
            hasRetentionTime=list_rt,
            usesInternalStandard=self.get_internal_standard(hplc_method_iri),
            rdfs_comment=comment
        )
        return hplc_method

    def process_raw_hplc_report(self, hplc_report_iri: str) -> HPLCReport:
        """Here we can assume that the required information are already provided by the previous agents."""
        hplc_report_path, hplc_report_extension = self.get_raw_hplc_report_path_and_extension(hplc_report_iri)
        # TODO test if need to download the file from here? or can be processed directly?
        # retrieve a list of points
        list_points = hplc.read_raw_hplc_report_file(hplc_report_iri=hplc_report_iri, file_path=hplc_report_path, filename_extension=hplc_report_extension)

        # get the instance of HPLCMethod
        hplc_method = self.get_hplc_method_given_hplc_report(hplc_report_iri)

        # map them to chromatogram point (qury phase component based on hplc method, and hplc)
        list_concentration = []
        list_phase_component = []
        list_chrom_pts= []
        for pt in list_points:
            # generate phase component
            ontospecies_species = self.get_matching_species_from_hplc_results(pt.get(ONTOHPLC_RETENTIONTIME), hplc_method)
            concentration = OntoCAPE_Molarity(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                hasValue=OntoCAPE_ScalarValue(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(hplc_report_iri),
                    # hasUnitOfMeasure=, # TODO
                    # numericalValue= # TODO
                )
            )
            phase_component = OntoCAPE_PhaseComponent(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                hasProperty=concentration,
                representsOccurenceOf=ontospecies_species
            )

            chrom_pt = ChromatogramPoint(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                indicatesComponent=phase_component,
                hasPeakArea=pt.get(ONTOHPLC_PEAKAREA),
                atRetentionTime=pt.get(ONTOHPLC_RETENTIONTIME)
            )
            list_concentration.append(concentration)
            list_phase_component.append(phase_component)
            list_chrom_pts.append(chrom_pt)

        composition = OntoCAPE_Composition(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(hplc_report_iri),
            comprisesDirectly=list_concentration
        )

        # create output chemical instance
        # generate instance_iri for output_chemical (so that it can be used in OntoCAPE_SinglePhase)
        output_chemical_iri = initialiseInstanceIRI(getNameSpace(hplc_report_iri), ONTORXN_OUTPUTCHEMICAL)
        output_chemical = OutputChemical(
            instance_iri=output_chemical_iri,
            thermodynamicBehaviour=OntoCAPE_SinglePhase(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                hasStateOfAggregation=OntoCAPE_liquid,
                isComposedOfSubsystem=list_phase_component,
                has_composition=composition,
                representsThermodynamicBehaviorOf=output_chemical_iri
            )
        )

        # create chemical solution instance
        chem_sol_response = self.performQuery("""SELECT ?sol ?vial WHERE{<%s> <%s> ?sol. ?sol <%s> ?vial.}""" % (
            hplc_report_iri, ONTOHPLC_GENERATEDFOR, ONTOVAPOURTEC_FILLS))
        if len(chem_sol_response) > 1:
            raise Exception("Multiple instances of ChemicalSolution identified for HPLCReport <%s>: %s" % (hplc_report_iri, str(chem_sol_response)))
        elif len(chem_sol_response) < 1:
            raise Exception("No instance of ChemicalSolution identified for HPLCReport <%s>" % hplc_report_iri)
        else:
            chem_sol_iri = chem_sol_response[0]['sol']
            chem_sol_vial = chem_sol_response[0]['vial']

        chemical_solution = ChemicalSolution(
            instance_iri=chem_sol_iri,
            refersToMaterial=output_chemical,
            fills=chem_sol_vial
        )

        # generate hplc report instance
        hplc_report_instance = HPLCReport(
            instance_iri=hplc_report_iri,
            hasReportPath=hplc_report_path,
            records=list_chrom_pts,
            generatedFor=chemical_solution
        )

        # TODO think about when do we write these triples (generated ones for CHromatogramPoint and ChemicalSolution) back to the knowledge graph
        return hplc_report_instance

    def get_chromatogram_point_of_hplc_report(self, hplc_report_iri: str) -> List[ChromatogramPoint]:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = PREFIX_RDF+"""SELECT ?pt ?phase_component ?chemical_species ?conc ?conc_type ?conc_value
                   ?conc_value ?conc_unit ?conc_num_val ?area ?area_value ?area_unit ?area_num_val
                   ?rt ?rt_value ?rt_unit ?rt_num_val
                   WHERE {
                       <%s> <%s> ?pt.
                       ?pt <%s> ?phase_component; <%s> ?area; <%s> ?rt.
                       ?phase_component <%s> ?chemical_species.
                       ?phase_component <%s> ?conc. ?conc rdf:type ?conc_type; <%s> ?conc_value.
                       ?conc_value <%s> ?conc_unit; <%s> ?conc_num_val.
                       ?area <%s> ?area_value. ?area_value <%s> ?area_unit; <%s> ?area_num_val.
                       ?rt <%s> ?rt_value. ?rt_value <%s> ?rt_unit; <%s> ?rt_num_val.
                   }""" % (
                       hplc_report_iri, ONTOHPLC_RECORDS, ONTOHPLC_INDICATESCOMPONENT, ONTOHPLC_HASPEAKAREA, ONTOHPLC_HASRETENTIONTIME,
                       ONTOCAPE_REPRESENTSOCCURENCEOF, ONTOCAPE_HASPROPERTY, ONTOCAPE_HASVALUE, ONTOCAPE_HASUNITOFMEASURE, ONTOCAPE_NUMERICALVALUE,
                       OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE
                   )
        response = self.performQuery(query)

        list_chrom_pts = []
        if len(response) != len(self.get_unique_values_in_list_of_dict(response, 'pt')):
            raise Exception("The ChromatogramPoint of the given HPLCReport <%s> is not uniquely stored: %s" % (
                hplc_report_iri, str(response)))
        for r in response:
            if r['conc_type'] == OntoCAPE_Molarity.__fields__['clz'].default:
                concentration = OntoCAPE_Molarity(instance_iri=r['conc'],hasValue=OntoCAPE_ScalarValue(instance_iri=r['conc_value'],numericalValue=r['conc_num_val'],hasUnitOfMeasure=r['conc_unit']))
            else:
                # TODO add support for other type of OntoCAPE_PhaseComponentConcentration
                raise NotImplementedError("Support for <%s> as OntoCAPE_PhaseComponentConcentration is NOT implemented yet." % r['conc_type'])
            pt = ChromatogramPoint(
                instance_iri=r['pt'],
                indicatesComponent=OntoCAPE_PhaseComponent(
                    instance_iri=r['phase_component'],
                    hasProperty=concentration,
                    representsOccurenceOf=r['chemical_species']
                ),
                hasPeakArea=PeakArea(
                    instance_iri=r['area'],
                    hasValue=OM_Measure(instance_iri=r['area_value'],hasUnit=r['area_unit'],hasNumericalValue=r['area_num_val'])
                ),
                atRetentionTime=RetentionTime(
                    instance_iri=r['rt'],
                    hasValue=OM_Measure(instance_iri=r['rt_value'],hasUnit=r['rt_unit'],hasNumericalValue=r['rt_num_val'])
                )
            )
            list_chrom_pts.append(pt)

        return list_chrom_pts

    def get_existing_hplc_report(self, hplc_report_iri: str) -> HPLCReport:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = """SELECT ?chemical_solution ?report_path ?local_report_file ?lastLocalModifiedAt ?lastUploadedAt ?vial
                   WHERE {<%s> <%s> ?chemical_solution; <%s> ?report_path; <%s> ?local_report_file; <%s> ?lastLocalModifiedAt; <%s> ?lastUploadedAt. ?chemical_solution <%s> ?vial.}""" % (
                       hplc_report_iri, ONTOHPLC_GENERATEDFOR, ONTOHPLC_HASREPORTPATH, ONTOHPLC_LOCALREPORTFILE, ONTOHPLC_LASTLOCALMODIFIEDAT, ONTOHPLC_LASTUPLOADEDAT, ONTOVAPOURTEC_FILLS)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple instances of ChemicalSolution or HPLCReport path identified for HPLCReport <%s>: %s" % (
                hplc_report_iri, str(response)
            ))
        elif len(response) < 1:
            raise Exception("No instances of ChemicalSolution or HPLCReport path identified for HPLCReport <%s>" % (hplc_report_iri))
        else:
            chem_sol_iri = response[0]['chemical_solution']
            hplc_report_path = response[0]['report_path']
            local_report_file = response[0]['local_report_file']
            lastLocalModifiedAt =  response[0]['lastLocalModifiedAt']
            lastUploadedAt = response[0]['lastUploadedAt']
            chem_sol_vial = response[0]['vial']

        hplc_report_instance = HPLCReport(
            instance_iri=hplc_report_iri,
            hasReportPath=hplc_report_path,
            localReportFile=local_report_file,
            lastLocalModifiedAt=lastLocalModifiedAt,
            lastUploadedAt=lastUploadedAt,
            records=self.get_chromatogram_point_of_hplc_report(hplc_report_iri),
            generatedFor=ChemicalSolution(
                instance_iri=chem_sol_iri,
                refersToMaterial=self.get_ontocape_material(chem_sol_iri, ONTOCAPE_REFERSTOMATERIAL, ONTORXN_OUTPUTCHEMICAL)[0],
                fills=chem_sol_vial
            )
        )

        return hplc_report_instance

    #######################################################
    ## Some utility functions handling the list and dict ##
    #######################################################
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

    def keep_wanted_keys_from_list_of_dict(self, list_of_dict: List[dict], wanted_keys: List[str]) -> list:
        return_list = []
        for one_dict in list_of_dict:
            return_list.append({key:one_dict[key] for key in wanted_keys})
        return return_list

    def remove_duplicate_dict_from_list_of_dict(self, list_of_dict: List[dict]) -> list:
        return [dict(t) for t in {tuple(sorted(d.items())) for d in list_of_dict}]

    def check_if_key_in_list_of_dict(self, list_of_dict: List[dict], key: str):
        for d in list_of_dict:
            if key in d:
                return True
        return False
