# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from datetime import datetime
from typing import Tuple
from rdflib import Graph
from rdflib import XSD
from pathlib import Path
import collections
import requests
from requests import status_codes
import uuid
import os

from pyderivationagent.kg_operations import PySparqlClient
from pyderivationagent.data_model import *

from chemistry_and_robots.data_model import *
from chemistry_and_robots.hardware import hplc

from chemistry_and_robots.kg_operations import dict_and_list as dal

import logging
logger = logging.getLogger('chemistry_and_robots_sparql_client')
logging.getLogger('py4j').setLevel(logging.INFO)

class ChemistryAndRobotsSparqlClient(PySparqlClient):

    def collect_triples_for_new_experiment(self, doe: DesignOfExperiment, newExp: List[ReactionExperiment]) -> Graph:
        """
            This method is used to collect the suggested new experiments as triples.
            It first serialises the ReactionVariation/ReactionExperiment instance to rdflib.Graph().
            It then add the triple that connects the OntoDoE:DesignOfExperiment instance and the new created OntoReaction:ReactionExperiment/ReactionVariation instance.

            Arguments:
                doe - instance of dataclass OntoDoE.DesignOfExperiment
                newExp - a list of instance of dataclass OntoReaction.ReactionExperiment
        """
        # (1) first serialise ReactionVariation/ReactionExperiment instance to rdflib.Graph()
        # All information should already be prepared and added to the instance
        # Method create_instance_for_kg will write all information to rdflib Graph on-the-fly
        g = Graph()
        # NOTE although here we loop through the list of OntoReaction:ReactionVariation/ReactionExperiment
        # NOTE in theory, the len(newExp) should be 1 (as we decided to make DoE Agent only suggest 1 experiment per derivation)
        # NOTE the loop is added for the future development
        for exp in newExp:
            g = exp.create_instance_for_kg(g)
            # (2) add triple that connects the OntoDoE:DesignOfExperiment with OntoReaction:ReactionVariation/ReactionExperiment
            g.add((URIRef(doe.instance_iri), URIRef(ONTODOE_PROPOSESNEWEXPERIMENT), URIRef(exp.instance_iri)))

        return g

    def get_doe_instance(self, doe_iri) -> DesignOfExperiment:
        doe_iri = trimIRI(doe_iri)
        query_1 = """SELECT ?strategy ?domain ?hist_data WHERE { <%s> <%s> ?strategy; <%s> ?domain; <%s> ?hist_data. }""" % (
            doe_iri, ONTODOE_USESSTRATEGY, ONTODOE_HASDOMAIN, ONTODOE_UTILISESHISTORICALDATA)
        response_1 = self.performQuery(query_1)
        if len(response_1) > 1:
            raise Exception("OntoDoE:DesignOfExperiment instance <%s> is associated with multiple entries of OntoDoE:Strategy/Domain/HistoricalData: %s" % (
                doe_iri, str(response_1)))
        elif len(response_1) < 1:
            raise Exception("OntoDoE:DesignOfExperiment instance <%s> is NOT associated with any entries of OntoDoE:Strategy/Domain/HistoricalData" % (doe_iri))
        else:
            r = response_1[0]

        query_2 = """SELECT ?sys_res WHERE { <%s> <%s> ?sys_res. }""" % (doe_iri, ONTODOE_HASSYSTEMRESPONSE)
        response_2 = self.performQuery(query_2)

        doe_instance = DesignOfExperiment(
            instance_iri=doe_iri,
            usesStrategy=self.getDoEStrategy(r['strategy']),
            hasDomain=self.getDoEDomain(r['domain']),
            hasSystemResponse=self.getSystemResponses([list(res.values())[0] for res in response_2]),
            utilisesHistoricalData=self.getDoEHistoricalData(r['hist_data']),
            proposesNewExperiment=self.getNewExperimentFromDoE(doe_iri)
        )
        return doe_instance

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
            numOfNewExp=num_[0], # TODO [when run in loop] what if no instance is in place?
            refersTo=self.getReactionExperiment(rxnexp_iris)
            )
        return historicalData_instance

    def getReactionExperiment(self, rxnexp_iris: str or list) -> List[ReactionExperiment]:
        """
            This method retrieves information given a list of instance iri of OntoReaction:ReactionExperiment.

            Arguments:
                rxnexp_iris - iri of OntoReaction:ReactionExperiment instance, can be either str of one instance, or a list of instances
        """

        if not isinstance(rxnexp_iris, list):
            rxnexp_iris = [rxnexp_iris]

        list_exp = []
        for exp_iri in rxnexp_iris:
            rdf_type_rxn = self.get_rdf_type_of_rxn_exp(exp_iri)
            if rdf_type_rxn == ONTOREACTION_REACTIONEXPERIMENT:
                list_exp.append(
                    ReactionExperiment(
                        instance_iri=exp_iri,
                        hasReactionCondition=self.getExpReactionCondition(exp_iri),
                        hasPerformanceIndicator=self.getExpPerformanceIndicator(exp_iri),
                        hasInputChemical=self.get_input_chemical_of_rxn_exp(exp_iri),
                        hasOutputChemical=self.get_output_chemical_of_rxn_exp(exp_iri),
                        isAssignedTo=self.get_r4_reactor_rxn_exp_assigned_to(exp_iri),
                        isOccurenceOf=self.get_chemical_reaction(exp_iri)
                    )
                )
            elif rdf_type_rxn == ONTOREACTION_REACTIONVARIATION:
                reference_reaction_exp = self.get_rxn_exp_iri_given_rxn_variation(exp_iri)
                list_exp.append(
                    ReactionVariation(
                        instance_iri=exp_iri,
                        hasReactionCondition=self.getExpReactionCondition(exp_iri),
                        hasPerformanceIndicator=self.getExpPerformanceIndicator(exp_iri),
                        hasInputChemical=self.get_input_chemical_of_rxn_exp(exp_iri),
                        hasOutputChemical=self.get_output_chemical_of_rxn_exp(exp_iri),
                        isAssignedTo=self.get_r4_reactor_rxn_exp_assigned_to(exp_iri),
                        isOccurenceOf=self.get_chemical_reaction(reference_reaction_exp),
                        isVariationOf=self.getReactionExperiment(reference_reaction_exp)[0]
                    )
                )
        return list_exp

    def get_rxn_exp_iri_given_rxn_variation(self, rxn_variation_iri: str) -> str:
        rxn_variation_iri = trimIRI(rxn_variation_iri)
        query = """SELECT ?rxn_exp WHERE { <%s> <%s> ?rxn_exp. }""" % (rxn_variation_iri, ONTOREACTION_ISVARIATIONOF)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("OntoReaction:ReactionVariation instance <%s> is found to be variation of multiple instance of OntoReaction:ReactionExperiment: %s" % (
                rxn_variation_iri, str(response)))
        elif len(response) < 1:
            raise Exception("OntoReaction:ReactionVariation instance <%s> is NOT found to be variation of any instance of OntoReaction:ReactionExperiment" % rxn_variation_iri)
        else:
            return [list(r.values())[0] for r in response][0]

    def get_chemical_reaction_of_rxn_variation(self, rxn_variation_iri: str) -> OntoCAPE_ChemicalReaction:
        rxn_variation_iri = trimIRI(rxn_variation_iri)
        reference_reaction_exp = self.get_rxn_exp_iri_given_rxn_variation(rxn_variation_iri)
        return self.get_chemical_reaction(reference_reaction_exp)

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
                   }""" % (ONTOKIN_SPECIES, ONTOKIN_REACTANT, ONTOKIN_SPECIES, ONTOKIN_PRODUCT, ONTOREACTION_TARGETPRODUCT, ONTOREACTION_IMPURITY,
                   rxnexp_iri, ONTOREACTION_ISOCCURENCEOF, ONTOCAPE_HASREACTANT, ONTOCAPE_HASPRODUCT,
                   ONTOSPECIES_HASUNIQUESPECIES, ONTOSPECIES_HASUNIQUESPECIES,
                   ONTOKIN_SPECIES, ONTOREACTION_CATALYST, ONTOCAPE_CATALYST, ONTOSPECIES_HASUNIQUESPECIES,
                   ONTOKIN_SPECIES, ONTOREACTION_SOLVENT, ONTOREACTION_HASSOLVENT, ONTOSPECIES_HASUNIQUESPECIES)
        response = self.performQuery(query)
        logger.debug(response)

        unique_chem_rxn = dal.get_unique_values_in_list_of_dict(response, 'chem_rxn')
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
        if not dal.check_if_key_in_list_of_dict(list_of_dict, species_role):
            return None
        else:
            wanted_info = dal.keep_wanted_keys_from_list_of_dict(list_of_dict, [species_role, species_type, ontospecies_key])
            wanted_info = dal.remove_duplicate_dict_from_list_of_dict(wanted_info)
            unique_species_iri = dal.get_unique_values_in_list_of_dict(wanted_info, species_role)
            for u_s in unique_species_iri:
                u_s_info = dal.get_sublist_in_list_of_dict_matching_key_value(wanted_info, species_role, u_s)
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
                elif u_s_info[species_type] == ONTOREACTION_TARGETPRODUCT:
                    species = TargetProduct(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOREACTION_IMPURITY:
                    species = Impurity(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOREACTION_CATALYST:
                    species = Catalyst(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOREACTION_SOLVENT:
                    species = Solvent(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                else:
                    raise Exception("Species type (%s) NOT supported for: %s" % (u_s_info[species_type], str(u_s_info)))
                list_species.append(species)

            return list_species

    def get_rdf_type_of_rxn_exp(self, rxnexp_iri: str) -> str:
        rxnexp_iri = trimIRI(rxnexp_iri)
        query = PREFIX_RDF + """SELECT ?type WHERE { VALUES ?type {<%s> <%s>}. <%s> rdf:type ?type. }""" % (ONTOREACTION_REACTIONEXPERIMENT, ONTOREACTION_REACTIONVARIATION, rxnexp_iri)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple rdf:type identified for reaction experiment <%s>: %s" % (rxnexp_iri, str(response)))
        elif len(response) < 1:
            raise Exception("Reaction experiment <%s> is missing rdf:type as either OntoReaction:ReactionExperiment or OntoReaction:ReactionVariation." % (rxnexp_iri))
        else:
            return response[0]['type']

    def get_input_chemical_of_rxn_exp(self, rxnexp_iri: str) -> List[InputChemical]:
        rxnexp_iri = trimIRI(rxnexp_iri)
        return self.get_ontocape_material(rxnexp_iri, ONTOREACTION_HASINPUTCHEMICAL)

    def get_output_chemical_of_rxn_exp(self, rxnexp_iri: str) -> List[OutputChemical]:
        rxnexp_iri = trimIRI(rxnexp_iri)
        return self.get_ontocape_material(rxnexp_iri, ONTOREACTION_HASOUTPUTCHEMICAL)

    def get_ontocape_material(self, subject_iri, predicate_iri, desired_type: str=None) -> List[OntoCAPE_Material]:
        subject_iri = trimIRI(subject_iri)
        predicate_iri = trimIRI(predicate_iri)

        if predicate_iri == ONTOREACTION_HASINPUTCHEMICAL:
            ocm_query_key = 'input_chemical'
        elif predicate_iri == ONTOREACTION_HASOUTPUTCHEMICAL:
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
            unique_ontocape_material_iri = dal.get_unique_values_in_list_of_dict(response, ocm_query_key)
            list_list_ontocape_material = []
            for iri in unique_ontocape_material_iri:
                list_list_ontocape_material.append([res for res in response if iri == res[ocm_query_key]])

            for list_om in list_list_ontocape_material:
                ontocape_material_iri = dal.get_unique_values_in_list_of_dict(list_om, ocm_query_key)[0] # here we are sure this is the unique value of OntoCAPE:Material

                # validate that the list of responses are only referring to one instance of OntoCAPE_SinglePhase, one instance of OntoCAPE_StateOfAggregation and one instance of OntoCAPE_Composition, otherwise raise an Exception
                unique_single_phase_iri = dal.get_unique_values_in_list_of_dict(list_om, 'single_phase')
                if len(unique_single_phase_iri) > 1:
                    raise Exception("Multiple thermodynamicBehavior OntoCAPE:SinglePhase identified (<%s>) in one instance of OntoReaction:InputChemical/OntoReaction:OutputChemical/OntoCAPE:Material %s is currently NOT supported." % ('>, <'.join(unique_single_phase_iri), ontocape_material_iri))
                elif len(unique_single_phase_iri) < 1:
                    raise Exception("No instance of thermodynamicBehavior OntoCAPE:SinglePhase was identified given instance of OntoReaction:InputChemical/OntoReaction:OutputChemical/OntoCAPE:Material: %s" % (ontocape_material_iri))
                else:
                    unique_single_phase_iri = unique_single_phase_iri[0]

                unique_state_of_aggregation_iri = dal.get_unique_values_in_list_of_dict(list_om, 'state_of_aggregation')
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

                unique_composition_iri = dal.get_unique_values_in_list_of_dict(list_om, 'composition')
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
                    if predicate_iri == ONTOREACTION_HASINPUTCHEMICAL:
                        ocm_instance = InputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                    elif predicate_iri == ONTOREACTION_HASOUTPUTCHEMICAL:
                        ocm_instance = OutputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                    else:
                        ocm_instance = OntoCAPE_Material(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                elif desired_type == ONTOREACTION_INPUTCHEMICAL:
                    ocm_instance = InputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)
                elif desired_type == ONTOREACTION_OUTPUTCHEMICAL:
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
                        OPTIONAL {?vial <%s> ?chemical_solution.}
                    }
                    """ % (
                        ONTOVAPOURTEC_AUTOSAMPLER, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY,
                        ONTOVAPOURTEC_HASSITE, ONTOVAPOURTEC_HOLDS, ONTOVAPOURTEC_LOCATIONID,
                        ONTOVAPOURTEC_HASFILLLEVEL, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                        ONTOVAPOURTEC_HASMAXLEVEL, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                        ONTOVAPOURTEC_ISFILLEDWITH
                    )

            response = self.performQuery(query)

            unique_autosampler_list = dal.get_unique_values_in_list_of_dict(response, 'autosampler')
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
                        OPTIONAL {?vial <%s> ?chemical_solution.}
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
            info_of_specific_autosampler = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'autosampler', specific_autosampler) if given_autosampler_iri is None else response

            unique_specific_autosampler_manufacturer_iri = dal.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'autosampler_manufacturer')
            if len(unique_specific_autosampler_manufacturer_iri) > 1:
                raise Exception("Multiple dbpedia:manufacturer identified (<%s>) in one instance of OntoVapourtec:AutoSampler %s is currently NOT supported." % ('>, <'.join(unique_specific_autosampler_manufacturer_iri), specific_autosampler))
            elif len(unique_specific_autosampler_manufacturer_iri) < 1:
                raise Exception("No instance of dbpedia:manufacturer was identified given instance of OntoVapourtec:AutoSampler: %s" % (specific_autosampler))
            else:
                unique_specific_autosampler_manufacturer_iri = unique_specific_autosampler_manufacturer_iri[0]

            unique_specific_autosampler_laboratory_iri = dal.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'laboratory')
            if len(unique_specific_autosampler_laboratory_iri) > 1:
                raise Exception("Multiple OntoLab:isContainedIn OntoLab:Laboratory identified (<%s>) for one instance of OntoVapourtec:AutoSampler %s." % ('>, <'.join(unique_specific_autosampler_laboratory_iri), specific_autosampler))
            elif len(unique_specific_autosampler_laboratory_iri) < 1:
                raise Exception("No instance of OntoLab:isContainedIn OntoLab:Laboratory was identified given instance of OntoVapourtec:AutoSampler: %s" % (specific_autosampler))
            else:
                unique_specific_autosampler_laboratory_iri = unique_specific_autosampler_laboratory_iri[0]

            unique_specific_autosampler_power_supply_iri = dal.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'autosampler_power_supply')
            if len(unique_specific_autosampler_power_supply_iri) > 1:
                raise Exception("Multiple OntoLab:hasPowerSupply OntoLab:PowerSupply identified (<%s>) for one instance of OntoVapourtec:AutoSampler %s is currently NOT supported." % ('>, <'.join(unique_specific_autosampler_power_supply_iri), specific_autosampler))
            elif len(unique_specific_autosampler_power_supply_iri) < 1:
                raise Exception("No instance of OntoLab:hasPowerSupply OntoLab:PowerSupply was identified given instance of OntoVapourtec:AutoSampler: %s" % (specific_autosampler))
            else:
                unique_specific_autosampler_power_supply_iri = unique_specific_autosampler_power_supply_iri[0]

            logger.debug("The sublist of all information related to the specific instance of OntoVapourtec:AutoSampler <%s> in the knowledge graph (%s): %s" %
                (specific_autosampler, self.kg_client.getQueryEndpoint(), str(info_of_specific_autosampler)))

            unique_site_list = dal.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'site')
            list_autosampler_site = []
            for specific_site in unique_site_list:
                info_of_specific_site = dal.get_sublist_in_list_of_dict_matching_key_value(info_of_specific_autosampler, 'site', specific_site)[0] # TODO here we assume only one, but to be varified by checking length
                # TODO add exceptions for lenth of the results, e.g. if len(info_of_specific_site) > 1:
                # TODO add logger info/debug
                if 'chemical_solution' in info_of_specific_site:
                    lst_referred_instance_of_ontocape_material = self.get_ontocape_material(info_of_specific_site['chemical_solution'], ONTOCAPE_REFERSTOMATERIAL)
                    referred_instance_of_ontocape_material = lst_referred_instance_of_ontocape_material[0] if lst_referred_instance_of_ontocape_material is not None else None # TODO here we assume only one, but to be varified by checking length
                else:
                    referred_instance_of_ontocape_material = None
                # TODO add exceptions for lenth of the results, e.g. if len(referred_instance_of_ontocape_material) > 1:
                vial = Vial(
                    instance_iri=info_of_specific_site['vial'],
                    isFilledWith=ChemicalSolution(
                        instance_iri=info_of_specific_site['chemical_solution'],
                        refersToMaterial=referred_instance_of_ontocape_material,
                        fills=info_of_specific_site['vial'],
                        isPreparedBy=None # TODO add support for isPreparedBy
                    ) if 'chemical_solution' in info_of_specific_site else None,
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
            This method retrieves a list of ReactionCondition pointed by the given instance of OntoReaction:ReactionExperiment/ReactionVariation.

            Arguments:
                rxnexp_iri - IRI of instance of OntoReaction:ReactionExperiment/ReactionVariation
        """

        # Delete "<" and ">" around the IRI
        rxnexp_iri = trimIRI(rxnexp_iri)

        # Construct query string
        query = PREFIX_RDFS + PREFIX_RDF + PREFIX_XSD + PREFIX_OWL + """SELECT DISTINCT ?condition ?subo ?clz ?measure ?val ?unit ?parameter_setting ?id ?multi ?usage \
                WHERE { ?subo rdfs:subPropertyOf* <%s> . <%s> ?subo ?condition .
                ?condition rdf:type ?clz . FILTER(?clz != owl:Thing && ?clz != owl:NamedIndividual && ?clz != <%s>) .
                ?condition <%s> ?measure . ?measure <%s> ?unit; <%s> ?val .
                OPTIONAL {?condition <%s> ?parameter_setting .} .
                OPTIONAL {?condition <%s> ?id .} .
                OPTIONAL {?condition <%s> ?multi .} .
                OPTIONAL {?condition <%s> ?usage .} .
                }""" % (ONTOREACTION_HASREACTIONCONDITION, rxnexp_iri, ONTOREACTION_REACTIONCONDITION,
                OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE, ONTOLAB_TRANSLATESTOPARAMETERSETTING, ONTODOE_POSITIONALID, ONTOREACTION_INDICATESMULTIPLICITYOF, ONTOREACTION_INDICATESUSAGEOF)

        # Perform SPARQL query
        response = self.performQuery(query)

        # Populate the list of ReactionCondition based on query results
        list_con = []

        unique_reaction_condition = dal.get_unique_values_in_list_of_dict(response, 'condition')
        dct_unique_rc = {rc:dal.get_sublist_in_list_of_dict_matching_key_value(response, 'condition', rc) for rc in unique_reaction_condition}
        for rc in unique_reaction_condition:
            _info = dct_unique_rc[rc]
            __clz = list(set([d['clz'] for d in _info]))
            if len(__clz) == 1:
                _clz = __clz[0]
            else:
                raise Exception("The rdf:type of ReactionCondition <%s> is not uniquely identified: %s" % (rc, __clz))
            _subo = list(set([d['subo'] for d in _info]))
            _id = list(set([d['id'] for d in _info if 'id' in d]))
            _measure = list(set([d['measure'] for d in _info if 'measure' in d]))
            _unit = list(set([d['unit'] for d in _info if 'unit' in d]))
            _val = list(set([d['val'] for d in _info if 'val' in d]))
            _parameter_setting = list(set([d['parameter_setting'] for d in _info if 'parameter_setting' in d]))
            _multi = list(set([d['multi'] for d in _info if 'multi' in d]))
            _usage = list(set([d['usage'] for d in _info if 'usage' in d]))

            rxn_condition = ReactionCondition(
                instance_iri=rc,
                clz=_clz,
                objPropWithExp=_subo,
                hasValue=OM_Measure(instance_iri=_measure[0],hasUnit=_unit[0],hasNumericalValue=_val[0]) if len(_measure) == 1 else None,
                positionalID=_id[0] if len(_id) == 1 else None,
                translateToParameterSetting=_parameter_setting[0] if len(_parameter_setting) == 1 else None,
                indicatesMultiplicityOf=_multi[0] if len(_multi) == 1 else None,
                indicateUsageOf=_usage[0] if len(_usage) == 1 else None
            )
            list_con.append(rxn_condition)

        return list_con

    def getExpPerformanceIndicator(self, rxnexp_iri: str) -> List[PerformanceIndicator]:
        """
            This method retrieves a list of PerformanceIndicator pointed by the given instance of OntoReaction:ReactionExperiment/ReactionVariation.

            Arguments:
                rxnexp_iri - IRI of instance of OntoReaction:ReactionExperiment/ReactionVariation
        """

        # Delete "<" and ">" around the IRI
        rxnexp_iri = trimIRI(rxnexp_iri)

        # Construct query string
        query = PREFIX_RDFS + PREFIX_RDF + PREFIX_XSD + PREFIX_OWL + """SELECT DISTINCT ?perf ?subo ?clz ?measure ?val ?unit ?id
                WHERE { ?subo rdfs:subPropertyOf* <%s> . <%s> ?subo ?perf .
                ?perf rdf:type ?clz . FILTER(?clz != owl:Thing && ?clz != owl:NamedIndividual && ?clz != <%s>) .
                OPTIONAL {?perf <%s> ?id .} .
                OPTIONAL {?perf <%s> ?measure . ?measure <%s> ?unit; <%s> ?val .} .
                }""" % (ONTOREACTION_HASPERFORMANCEINDICATOR, rxnexp_iri, ONTOREACTION_PERFORMANCEINDICATOR, ONTODOE_POSITIONALID,
                OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        # Perform SPARQL query
        response = self.performQuery(query)

        if len(response) == 0:
            logger.info("ReactionExperiment/ReactionVariation <%s> has no PerformanceIndicator computed yet" % (rxnexp_iri))
            return None
        else:
            # Populate the list of PerformanceIndicator based on query results
            list_perf = []
            unique_performance_indicator = dal.get_unique_values_in_list_of_dict(response, 'perf')
            dct_unique_pi = {pi:dal.get_sublist_in_list_of_dict_matching_key_value(response, 'perf', pi) for pi in unique_performance_indicator}
            for pi in unique_performance_indicator:
                _info = dct_unique_pi[pi]
                __clz = list(set([d['clz'] for d in _info]))
                if len(__clz) == 1:
                    _clz = __clz[0]
                else:
                    raise Exception("The rdf:type of PerformanceIndicator <%s> is not uniquely identified: %s" % (pi, __clz))
                _subo = list(set([d['subo'] for d in _info]))
                _id = list(set([d['id'] for d in _info if 'id' in d]))
                _measure = list(set([d['measure'] for d in _info if 'measure' in d]))
                _unit = list(set([d['unit'] for d in _info if 'unit' in d]))
                _val = list(set([d['val'] for d in _info if 'val' in d]))
                perf_indicator = PerformanceIndicator(
                    instance_iri=pi,
                    clz=_clz,
                    rxn_exp_iri=rxnexp_iri,
                    objPropWithExp=_subo,
                    hasValue=OM_Measure(instance_iri=_measure[0],hasUnit=_unit[0],hasNumericalValue=_val[0]) if len(_measure) == 1 else None,
                    positionalID=_id[0] if len(_id) == 1 else None
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

        if (res[0] == ONTODOE_TSEMO):
            tsemo_instance = self.getTSEMOSettings(strategy_iri)
            return tsemo_instance
        elif (res[0] == ONTODOE_LHS):
            # TODO implement handling for LHS
            raise NotImplementedError("LHS as a OntoDoE:Strategy is not yet supported.")
        else:
            # TODO implement handling for other DoE strategy
            raise NotImplementedError("<%s> as a OntoDoE:Strategy is not yet supported." % (res[0]))

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

    def getNewExperimentFromDoE(self, doe_iri: str) -> ReactionExperiment:
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
        elif (len(response) == 1):
            return self.getReactionExperiment(response[0]['newexp'])[0]
        return None

    def create_equip_settings_for_rs400_from_rxn_exp(self, rxnexp: ReactionExperiment, rs400: VapourtecRS400, preferred_r4_reactor: VapourtecR4Reactor) -> List[EquipmentSettings]:
        list_equip_setting = []
        reactor_setting = ReactorSettings(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            hasResidenceTimeSetting=ResidenceTimeSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=self.get_rxn_con_or_perf_ind(rxnexp.hasReactionCondition, ONTOREACTION_RESIDENCETIME),
                namespace_for_init=getNameSpace(rxnexp.instance_iri)
            ),
            hasReactorTemperatureSetting=ReactorTemperatureSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=self.get_rxn_con_or_perf_ind(rxnexp.hasReactionCondition, ONTOREACTION_REACTIONTEMPERATURE),
                namespace_for_init=getNameSpace(rxnexp.instance_iri)
            ),
            namespace_for_init=getNameSpace(rxnexp.instance_iri),
            specifies=preferred_r4_reactor,
            wasGeneratedFor=rxnexp.instance_iri
        )

        list_equip_setting.append(reactor_setting)

        # first identify the reference reactant
        dict_stoi_ratio = {cond.indicatesMultiplicityOf:cond for cond in rxnexp.hasReactionCondition if cond.clz == ONTOREACTION_STOICHIOMETRYRATIO}
        reference_input_chemical = [chem for chem in dict_stoi_ratio if abs(dict_stoi_ratio.get(chem).hasValue.hasNumericalValue - 1) < 0.000001]
        if len(reference_input_chemical) > 1:
            raise Exception("Multiple reference chemicals are identified: " + str(reference_input_chemical) + " for reaction experiment: " + str(rxnexp.dict()))
        elif len(reference_input_chemical) < 1:
            raise Exception("No reference chemicals are identified for reaction experiment: " + str(rxnexp.dict()))
        else:
            reference_input_chemical = reference_input_chemical[0]

        reaction_scale = [cond for cond in rxnexp.hasReactionCondition if cond.clz == ONTOREACTION_REACTIONSCALE]
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
            if site.holds.isFilledWith is not None:
                if site.holds.isFilledWith.refersToMaterial is None:
                    logger.warning("ChemicalSolution exist but NO Material is specified for autosampler site <%s>." % site.instance_iri)
                elif site.holds.isFilledWith.refersToMaterial.thermodynamicBehaviour == input_chem.thermodynamicBehaviour:
                    return site
        return None

    def get_autosampler_from_vapourtec_rs400(self, rs400: VapourtecRS400) -> AutoSampler:
        # NOTE this method assumes there's only one AutoSampler associated with one VapourtecRS400 module
        for equip in rs400.consistsOf:
            if isinstance(equip, AutoSampler):
                return equip
        return None

    def sort_r2_pumps_in_vapourtec_rs400(self, rs400: VapourtecRS400) -> Dict[str, VapourtecR2Pump]:
        dict_pumps = {pump.locationID:pump for pump in rs400.consistsOf if isinstance(pump, VapourtecR2Pump)}
        return {key:dict_pumps[key] for key in sorted(dict_pumps)}

    def collect_triples_for_equip_settings(self, equip_settings: List[EquipmentSettings], configure_digital_twin: bool):
        g = Graph()
        for es in equip_settings:
            g = es.create_instance_for_kg(g, configure_digital_twin)

        return g

    def get_rxn_con_or_perf_ind(self, list_: List[ReactionCondition] or List[PerformanceIndicator], clz, positionalID=None):
        var = []
        for l in list_:
            # TODO [modify when run in loop] what if unknown positionalID when calling this function? in fact, does positionalID matter?
            logger.debug(l.__dict__)
            if tuple((l.clz, l.positionalID)) == tuple((clz, positionalID)):
                var.append(l)
        if len(var) > 1:
            raise Exception("Only one appearance should be allowed for a ReactionCondition/PerformanceIndicator to be set as a ParameterSetting, found: <%s>." % '>, <'.join(var))
        elif len(var) < 1:
            raise Exception("Class <%s> was not found." % clz)
            # raise Exception(len(list_))
        else:
            logger.debug("Identified quantity")
            logger.debug(var[0])
            return var[0]

    def get_preferred_vapourtec_rs400(self, rxnexp: ReactionExperiment) -> Tuple[VapourtecRS400, VapourtecR4Reactor, HPLC]:
        """ This function queries the digital twin of the most suitable VapourtecRS400 for the given reaction experiment."""
        # first step: query if suitable chemicals given the experiment --> does the vial hold the chemicals that has the same thermodynamicBehaviour as the InputChemical of a reaction experiment
        list_autosampler = self.get_all_autosampler_with_fill()
        list_input_chemical = self.get_input_chemical_of_rxn_exp(rxnexp.instance_iri)
        list_input_chemical_single_phase = [input_chem.thermodynamicBehaviour for input_chem in list_input_chemical]
        for autosampler in list_autosampler:
            list_chemical_solution_mat_single_phase = [site.holds.isFilledWith.refersToMaterial.thermodynamicBehaviour for site in autosampler.hasSite if site.holds.isFilledWith is not None]
            logger.debug([sp.dict() for sp in list_chemical_solution_mat_single_phase])
            logger.debug([sp.dict() for sp in list_input_chemical_single_phase])
            if all(item in list_chemical_solution_mat_single_phase for item in list_input_chemical_single_phase):
                # second step: query if the operation range covers the reaction condition
                # NOTE here we only consider the reaction temperature at the moment, support for checking more reaction conditions (e.g. reactor material, autosampler liquid level, etc.) can be added later on
                # get the reactor first, then get the temperature conditon (cached value), compare the value if in the range, if yes then carry out the next step (how many prior experiment), otherwise skip
                vapourtec_rs400 = self.get_vapourtec_rs400_given_autosampler(autosampler)
                list_reactor = [lab_equip for lab_equip in vapourtec_rs400.consistsOf if isinstance(lab_equip, VapourtecR4Reactor)]

                temp_condition = [cond.hasValue.hasNumericalValue for cond in rxnexp.hasReactionCondition if cond.clz == ONTOREACTION_REACTIONTEMPERATURE]
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
                            if vapourtec_rs400.hasState.clz == ONTOVAPOURTEC_IDLE:
                                # TODO here we may provide functions for other analytical equipment
                                associated_hplc = self.get_hplc_given_vapourtec_rs400(vapourtec_rs400.instance_iri)
                                if vapourtec_rs400.isManagedBy is not None and associated_hplc.isManagedBy is not None:
                                    # TODO here we can add functions to inform the owner of hardware to spin up agent for execution
                                    return vapourtec_rs400, reactor, associated_hplc

        return None, None, None

    def update_vapourtec_rs400_state(self, vapourtec_rs400_iri: str, target_state: str, timestamp: float):
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        target_state = trimIRI(target_state)
        if target_state in LIST_ONTOVAPOURTEC_VALIDSTATE:
            update = PREFIX_RDF + """DELETE {?state rdf:type ?stateType. ?state <%s> ?timestamp.}
                    INSERT {?state rdf:type <%s>. ?state <%s> %f.}
                    WHERE {<%s> <%s> ?state. ?state rdf:type ?stateType; <%s> ?timestamp.}""" % (
                        ONTOLAB_STATELASTUPDATEDAT, target_state,
                        ONTOLAB_STATELASTUPDATEDAT, timestamp,
                        vapourtec_rs400_iri, SAREF_HASSTATE, ONTOLAB_STATELASTUPDATEDAT
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

    def get_vapourtec_rs400(self, vapourtec_rs400_iri: str) -> VapourtecRS400:
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        query = PREFIX_RDF + \
                """
                SELECT ?rs400_manufacturer ?laboratory ?rs400_power_supply ?state ?state_type ?last_update ?autosampler ?is_managed_by
                WHERE { <%s> <%s> ?autosampler; <%s> ?rs400_manufacturer; <%s> ?laboratory; <%s> ?rs400_power_supply; <%s> ?state. ?state a ?state_type; <%s> ?last_update. ?autosampler rdf:type <%s>. 
                OPTIONAL{<%s> <%s> ?is_managed_by.}
                }""" % (
                    vapourtec_rs400_iri, SAREF_CONSISTSOF, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY, SAREF_HASSTATE, ONTOLAB_STATELASTUPDATEDAT, ONTOVAPOURTEC_AUTOSAMPLER,
                    vapourtec_rs400_iri, ONTOLAB_ISMANAGEDBY
                )

        response = self.performQuery(query)

        # NOTE here we are assuming one VapourtecRS400 module has only ONE manufacturer, locates in only ONE laboratory, also has only ONE type of power supply
        # NOTE this might not hold universally, but we will simplify for the moment
        if len(response) > 1:
            raise Exception("One VapourtecRS400 (%s) should only consist of one AutoSampler module. Identified multiple: %s" % (vapourtec_rs400_iri, str(response)))
        elif len(response) < 1:
            raise Exception("Information associated with VapourtecRS400 (%s) is not complete when querying: %s" % (vapourtec_rs400_iri, query))
        else:
            res = response[0]

        list_vapourtec_reactor_and_pump = []
        list_vapourtec_reactor_and_pump.append(self.get_autosampler(res['autosampler']))
        list_vapourtec_reactor_and_pump += self.get_r4_reactor_given_vapourtec_rs400(vapourtec_rs400_iri)
        list_vapourtec_reactor_and_pump += self.get_r2_pump_given_vapourtec_rs400(vapourtec_rs400_iri)

        vapourtec_rs400 = VapourtecRS400(
            instance_iri=vapourtec_rs400_iri,
            manufacturer=res['rs400_manufacturer'],
            isContainedIn=res['laboratory'],
            hasPowerSupply=res['rs400_power_supply'],
            isManagedBy=res['is_managed_by'] if 'is_managed_by' in res else None,
            consistsOf=list_vapourtec_reactor_and_pump,
            hasState=VapourtecState(
                instance_iri=res['state'],
                clz=res['state_type'],
                stateLastUpdatedAt=res['last_update']
            )
        )

        return vapourtec_rs400

    def get_vapourtec_rs400_given_autosampler(self, autosampler: AutoSampler) -> VapourtecRS400:
        query = PREFIX_RDF + \
                """
                SELECT ?rs400 ?rs400_manufacturer ?laboratory ?rs400_power_supply ?state ?state_type ?last_update ?is_managed_by
                WHERE { ?rs400 <%s> <%s>; rdf:type <%s>; <%s> ?rs400_manufacturer; <%s> ?laboratory; <%s> ?rs400_power_supply; <%s> ?state. ?state a ?state_type; <%s> ?last_update. 
                OPTIONAL{?rs400 <%s> ?is_managed_by.}
                }""" % (
                    SAREF_CONSISTSOF, autosampler.instance_iri,
                    ONTOVAPOURTEC_VAPOURTECRS400, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY, SAREF_HASSTATE, ONTOLAB_STATELASTUPDATEDAT,
                    ONTOLAB_ISMANAGEDBY
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
            isManagedBy=res['is_managed_by'] if 'is_managed_by' in res else None,
            consistsOf=list_vapourtec_reactor_and_pump,
            hasState=VapourtecState(
                instance_iri=res['state'],
                clz=res['state_type'],
                stateLastUpdatedAt=res['last_update']
            )
        )

        return vapourtec_rs400

    def get_rxn_exp_assigned_to_r4_reactor(self, r4_reactor_iri: str) -> List[str]:
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        query = """
                SELECT ?rxnexp WHERE {?rxnexp <%s> <%s>.}
                """ % (ONTOREACTION_ISASSIGNEDTO, r4_reactor_iri)
        response = self.performQuery(query)
        list_rxn = [res['rxnexp'] for res in response]
        return list_rxn

    def get_r4_reactor_rxn_exp_assigned_to(self, rxn_exp_iri: str) -> str:
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        query = """SELECT ?r4_reactor WHERE { <%s> <%s> ?r4_reactor. }""" % (rxn_exp_iri, ONTOREACTION_ISASSIGNEDTO)
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
        unique_r4_reactor_list = dal.get_unique_values_in_list_of_dict(response, 'r4_reactor')
        logger.debug("The list of all OntoVapourtec:VapourtecR4Reactor associated with the given instance of OntoVapourtec:VapourtecRS400 <%s>: %s" % (
            vapourtec_rs400_iri, str(unique_r4_reactor_list)
        ))
        list_r4_reactor = []
        for specific_r4_reactor in unique_r4_reactor_list:
            info_of_specific_r4_reactor = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'r4_reactor', specific_r4_reactor)
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
        unique_r2_pump_list = dal.get_unique_values_in_list_of_dict(response, 'r2_pump')
        logger.debug("The list of all OntoVapourtec:VapourtecR2Pump associated with the given instance of OntoVapourtec:VapourtecRS400 <%s>: %s" % (
            vapourtec_rs400_iri, str(unique_r2_pump_list)
        ))
        list_r2_pump = []
        for specific_r2_pump in unique_r2_pump_list:
            info_of_specific_r2_pump = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'r2_pump', specific_r2_pump)

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
        update = """INSERT DATA {<%s> <%s> <%s>}""" % (rxn_exp_iri, ONTOREACTION_ISASSIGNEDTO, r4_reactor_iri)
        self.performUpdate(update)
        logger.info("ReactionExperiment <%s> is now assigned to VapourtecR4Reactor <%s>." % (rxn_exp_iri, r4_reactor_iri))

    def remove_rxn_exp_from_r4_reactor(self, rxn_exp_iri: str, r4_reactor_iri: str):
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        update = """DELETE DATA {<%s> <%s> <%s>}""" % (rxn_exp_iri, ONTOREACTION_ISASSIGNEDTO, r4_reactor_iri)
        self.performUpdate(update)
        logger.info("ReactionExperiment <%s> is no longer assigned to VapourtecR4Reactor <%s>." % (rxn_exp_iri, r4_reactor_iri))

    def get_prior_rxn_exp_in_queue(self, rxn_exp_iri: str, vapourtec_execution_agent_iri: str) -> Dict[str, int]:
        """This method queries the instances of ReactionExperiment that are prior in the queue for execution.
        NOTE: It is assumed there is only ONE possible OntoAgent:Service of DoE Agent. This can be extended if deemed necessary in the future."""
        # TODO support query prior experiments in the situation of multiple DoE Agent available
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        vapourtec_execution_agent_iri = trimIRI(vapourtec_execution_agent_iri)
        query = PREFIX_RDF + """
                SELECT ?rxn ?timestamp
                WHERE {
                    <%s> <%s> ?dd. ?dd <%s> ?doe_agent; <%s>/<%s>/<%s> ?specific_timestamp.
                    VALUES ?type {<%s> <%s>}.
                    ?rxn rdf:type ?type; <%s> ?doe_derivation.
                    ?doe_derivation <%s> ?doe_agent; <%s>/<%s>/<%s> ?timestamp.
                    ?rxn ^<%s> ?exe_derivation.
                    ?exe_derivation <%s> <%s>.
                    ?exe_derivation <%s>/rdf:type ?status_type.
                    filter(?timestamp < ?specific_timestamp)
                }
                """ % (rxn_exp_iri, ONTODERIVATION_BELONGSTO, ONTODERIVATION_ISDERIVEDUSING, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION,
                ONTOREACTION_REACTIONEXPERIMENT, ONTOREACTION_REACTIONVARIATION, ONTODERIVATION_BELONGSTO,
                ONTODERIVATION_ISDERIVEDUSING, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION,
                ONTODERIVATION_ISDERIVEDFROM, ONTODERIVATION_ISDERIVEDUSING, vapourtec_execution_agent_iri, ONTODERIVATION_HASSTATUS)
        logger.debug(query)
        response = self.performQuery(query)
        rxn_exp_queue = {res['rxn']:res['timestamp'] for res in response}
        return rxn_exp_queue

    def detect_new_hplc_report(self, hplc_digital_twin, start_timestamp, end_timestamp):
        query = """SELECT ?hplc_report WHERE { <%s> <%s> ?hplc_report. ?hplc_report <%s> ?timestamp.
                FILTER(?timestamp > %f && ?timestamp < %f)}""" % (
            hplc_digital_twin, ONTOHPLC_HASPASTREPORT, ONTOHPLC_LASTUPLOADEDAT, start_timestamp, end_timestamp)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple HPLCReport found between given period of time for HPLC <%s>: %s" % (hplc_digital_twin, str(response)))
        elif len(response) < 1:
            return None
        else:
            return response[0]['hplc_report']

    def collect_triples_for_hplc_job(
        self,
        rxn_exp_iri, chemical_solution_iri,
        hplc_digital_twin, hplc_report_iri, hplc_method_iri,
        g: Graph
    ):
        hplc_job_iri = initialiseInstanceIRI(getNameSpace(hplc_digital_twin), ONTOHPLC_HPLCJOB)
        logger.info("The initialised HPLCJob IRI is: <%s>" % (hplc_job_iri))

        hplc_method_iri = trimIRI(hplc_method_iri)
        logger.info("The HPLCReport <%s> was generated using HPLCMethod <%s>" % (hplc_report_iri, hplc_method_iri))

        g.add((URIRef(hplc_digital_twin), URIRef(ONTOHPLC_HASJOB), URIRef(hplc_job_iri)))
        g.add((URIRef(hplc_job_iri), RDF.type, URIRef(ONTOHPLC_HPLCJOB)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_CHARACTERISES), URIRef(rxn_exp_iri)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_USESMETHOD), URIRef(hplc_method_iri)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_HASREPORT), URIRef(hplc_report_iri)))
        g.add((URIRef(hplc_report_iri), URIRef(ONTOHPLC_GENERATEDFOR), URIRef(chemical_solution_iri)))
        return g

    def upload_raw_hplc_report_to_kg(self, local_file_path, timestamp_last_modified, remote_report_subdir, hplc_digital_twin) -> str:
        try:
            remote_file_path, timestamp_upload = self.uploadFile(local_file_path, remote_report_subdir)
            logger.info("HPLC raw report (%s) was uploaded to fileserver <%s> at %f with remote file path at: %s " % (
                    local_file_path, self.fs_url, timestamp_upload, remote_file_path))
        except Exception as e:
            logger.error(e)
            # TODO need to think a way to inform the post proc agent about the failure of uploading the file
            # TODO [when run in loop] repeat the upload process until the file is uploaded successfully?
            raise Exception("HPLC raw report (%s) upload failed with code %d" % (local_file_path))

        hplc_report_iri = initialiseInstanceIRI(getNameSpace(hplc_digital_twin), ONTOHPLC_HPLCREPORT)
        logger.info("The initialised HPLCReport IRI is: <%s>" % (hplc_report_iri))

        update = PREFIX_XSD + """INSERT DATA {<%s> a <%s>; <%s> "%s"^^xsd:anyURI; <%s> "%s"^^xsd:string; <%s> %f; <%s> %f. <%s> <%s> <%s>.}""" % (
            hplc_report_iri, ONTOHPLC_HPLCREPORT, ONTOHPLC_REMOTEFILEPATH, remote_file_path,
            ONTOHPLC_LOCALFILEPATH, local_file_path, ONTOHPLC_LASTLOCALMODIFIEDAT, timestamp_last_modified,
            ONTOHPLC_LASTUPLOADEDAT, timestamp_upload, hplc_digital_twin, ONTOHPLC_HASPASTREPORT, hplc_report_iri
        )
        self.performUpdate(update)

        return hplc_report_iri

    def register_agent_with_hardware(self, agent_iri, hardware_digital_twin):
        g = Graph()
        g.add((URIRef(hardware_digital_twin), URIRef(ONTOLAB_ISMANAGEDBY), URIRef(agent_iri)))
        # NOTE SPARQL update with sub-query to ensure same triple don't get written to KG (although this should not cause any issues)
        update = """INSERT { %s } WHERE { FILTER NOT EXISTS { %s } }""" % (
            g.serialize(format='nt'), g.serialize(format='nt')
        )
        self.performUpdate(update)

    def identify_rxn_exp_when_uploading_hplc_report(self, hplc_digital_twin: str, hplc_remote_file_path: str) -> str:
        hplc_digital_twin = trimIRI(hplc_digital_twin)
        query = """SELECT DISTINCT ?rxn_exp WHERE {<%s> ^<%s>/<%s>+/<%s>/<%s> ?rxn_exp.}""" % (
            hplc_digital_twin, SAREF_CONSISTSOF, SAREF_CONSISTSOF, ONTOLAB_ISSPECIFIEDBY, ONTOLAB_WASGENERATEDFOR)
        response = self.performQuery(query)
        rxn_exp = [list(res.values())[0] for res in response]
        if len(rxn_exp) > 1:
            raise Exception("Multiple instances of ReactionExperiment is identified to be associated with HPLC <%s> when uploading HPLCReport <%s>: %s" % (
                hplc_digital_twin, hplc_remote_file_path, str(response)))
        elif len(rxn_exp) < 1:
            raise Exception("No instance of ReactionExperiment is identified to be associated with HPLC <%s> when uploading HPLCReport <%s>" % (
                hplc_digital_twin, hplc_remote_file_path))
        else:
            return rxn_exp[0]

    def get_raw_hplc_report_remote_path_and_extension(self, hplc_report_iri: str) -> Tuple[str, str]:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = """SELECT ?remote_path ?extension WHERE {<%s> <%s> ?remote_path . ?hplc <%s>/<%s> <%s> . ?hplc <%s> ?extension .}""" % (
            hplc_report_iri, ONTOHPLC_REMOTEFILEPATH, ONTOHPLC_HASJOB, ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_REPORTEXTENSION)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple matches for filepath and extension for raw HPLC report <%s> was found: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No raw HPLC report found for <%s> when querying: %s" % (hplc_report_iri, query))
        else:
            remote_file_path = response[0]['remote_path']
            try:
                file_extension = MAPPING_FILENAMEEXTENSION.get(response[0]['extension'])
            except:
                raise NotImplementedError("Retrieving raw HPLC report with a file extension <%s> is NOT yet supported, associated HPLCReport iri <%s>, remote file path <%s>" % (
                    response[0]['extension'], hplc_report_iri, remote_file_path))
        return remote_file_path, file_extension

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
            raise Exception("InternalStandard NOT found for HPLCMethod <%s> when querying: %s" % (hplc_method_iri, query))
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

    def get_species_molar_mass_kilogrampermole(self, species_iri: str) -> float:
        """This method returns the molecular weight of the given OntoSpecies:Species iri in the unit of kg/mol."""
        species_iri = trimIRI(species_iri)
        query = """SELECT ?unit ?value WHERE { <%s> <%s> ?mw. ?mw <%s> ?unit; <%s> ?value. }""" % (
            species_iri, ONTOSPECIES_HASMOLECULARWEIGHT, ONTOSPECIES_UNITS, ONTOSPECIES_VALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of MolecularWeight were identified for OntoSpecies:Species <%s>: %s" % (species_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of MolecularWeight was identified for OntoSpecies:Species <%s>" % (species_iri))
        else:
            if response[0]['unit'] == 'g/mol':
                return float(response[0]['value']) / 1000
            elif response[0]['unit'] == 'kg/mol':
                return float(response[0]['value'])
            else:
                raise NotImplementedError("Record of MolecularWeight in unit of <%s> is NOT yet supported for OntoSpecies:Species <%s>" % (
                    response[0]['unit'], species_iri))

    # TODO replace this with the proper representation of the species density - at given temperature, what's the density of the given species
    def get_species_density(self, species_iri: str) -> Tuple[float, str]:
        PLACEHOLDER_HASDENSITY = 'http://www.placeholder.com/for_species/hasDensity'
        species_iri = trimIRI(species_iri)
        query = """SELECT ?value ?unit WHERE { <%s> <%s>/<%s> ?density. ?density <%s> ?unit; <%s> ?value. }""" % (
            species_iri, PLACEHOLDER_HASDENSITY, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of Density were identified for OntoSpecies:Species <%s>: %s" % (species_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of Density was identified for OntoSpecies:Species <%s>" % (species_iri))
        else:
            return float(response[0]['value']), response[0]['unit']

    # TODO replace this with the proper representation of the material cost - with the given vendor, what's the cost of the given chemical
    def get_species_material_cost(self, species_iri: str) -> Tuple[float, str]:
        PLACEHOLDER_HASMATERIALCOST = 'http://www.placeholder.com/for_species/hasMaterialCost'
        species_iri = trimIRI(species_iri)
        query = """SELECT ?value ?unit WHERE { <%s> <%s>/<%s> ?cost. ?cost <%s> ?unit; <%s> ?value. }""" % (
            species_iri, PLACEHOLDER_HASMATERIALCOST, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of MaterialCost were identified for OntoSpecies:Species <%s>: %s" % (species_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of MaterialCost was identified for OntoSpecies:Species <%s>" % (species_iri))
        else:
            return float(response[0]['value']), response[0]['unit']

    # TODO replace this with the proper representation of the eco score - not sure if this is an "intrinsic" property of a given chemical
    def get_species_eco_score(self, species_iri: str) -> Tuple[float, str]:
        PLACEHOLDER_HASECOSCORE = 'http://www.placeholder.com/for_species/hasEcoScore'
        species_iri = trimIRI(species_iri)
        query = """SELECT ?value ?unit WHERE { <%s> <%s>/<%s> ?ecoscore. ?ecoscore <%s> ?unit; <%s> ?value. }""" % (
            species_iri, PLACEHOLDER_HASECOSCORE, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of EcoScore were identified for OntoSpecies:Species <%s>: %s" % (species_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of EcoScore was identified for OntoSpecies:Species <%s>" % (species_iri))
        else:
            return float(response[0]['value']), response[0]['unit']

    def get_reactor_volume_given_reactor(self, reactor_iri: str) -> Tuple[float, str]:
        reactor_iri = trimIRI(reactor_iri)
        query = """SELECT ?unit ?value WHERE { <%s> <%s>/<%s> ?measure. ?measure <%s> ?unit; <%s> ?value. }""" % (
            reactor_iri, ONTOVAPOURTEC_HASREACTORVOLUME, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of ReactorVolume were identified for VapourtecR4Reactor <%s>: %s" % (reactor_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of ReactorVolume was identified for VapourtecR4Reactor <%s>" % (reactor_iri))
        else:
            return float(response[0]['value']), response[0]['unit']

    def get_rxn_exp_associated_with_hplc_report(self, hplc_report_iri: str) -> ReactionExperiment:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = """SELECT ?rxn_exp WHERE { ?hplc_job <%s> <%s>; <%s> ?rxn_exp. }""" % (ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_CHARACTERISES)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of ReactionExperiment were identified for HPLCReport <%s>: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of ReactionExperiment was identified for HPLCReport <%s>" % (hplc_report_iri))
        else:
            rxn_exp_iri = response[0]['rxn_exp']

        return self.getReactionExperiment(rxn_exp_iri)[0]

    def get_internal_standard_associated_with_hplc_report(self, hplc_report_iri: str) -> InternalStandard:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = """SELECT ?hplc_method WHERE { ?hplc_job <%s> <%s>; <%s> ?hplc_method. }""" % (ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_USESMETHOD)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of HPLEMethod were identified for HPLCReport <%s>: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of HPLCMethod was identified for HPLCReport <%s>" % (hplc_report_iri))
        else:
            hplc_method_iri = response[0]['hplc_method']

        return self.get_internal_standard(hplc_method_iri)

    def process_raw_hplc_report(self, hplc_report_iri: str, internal_standard_species: str, internal_standard_run_conc_moleperlitre: float, temp_local_folder: str=None) -> HPLCReport:
        """Here we can assume that the required information are already provided by the previous agents."""
        remote_hplc_report_path, hplc_report_extension = self.get_raw_hplc_report_remote_path_and_extension(hplc_report_iri)

        if temp_local_folder is None: temp_local_folder = str(Path(__file__).absolute().parent)
        temp_local_file_path = os.path.join(temp_local_folder,f'{str(uuid.uuid4())}.'+hplc_report_extension)
        self.download_remote_raw_hplc_report(remote_hplc_report_path, temp_local_file_path)

        # retrieve a list of points
        list_points = hplc.read_raw_hplc_report_file(hplc_report_iri=hplc_report_iri, file_path=temp_local_file_path, filename_extension=hplc_report_extension)

        # get the instance of HPLCMethod
        hplc_method = self.get_hplc_method_given_hplc_report(hplc_report_iri)
        # create a dict for response factor
        dct_response_factor = {rf.refersToSpecies:rf.hasValue.hasNumericalValue for rf in hplc_method.hasResponseFactor}

        # map them to chromatogram point (qury phase component based on hplc method, and hplc)
        list_concentration = []
        list_phase_component = []
        list_chrom_pts= []
        dct_points = {self.get_matching_species_from_hplc_results(pt.get(ONTOHPLC_RETENTIONTIME), hplc_method):pt for pt in list_points}
        try:
            internal_standard_peak_area = dct_points[internal_standard_species][ONTOHPLC_PEAKAREA].hasValue.hasNumericalValue
        except KeyError:
            raise Exception("InternalStandard <%s> is NOT identified in the end stream associated with HPLCReport <%s>" % (internal_standard_species, hplc_report_iri))

        for pt in dct_points:
            # calculate concentration based on the peak area and response factor
            try:
                conc_num_val = dct_points[pt][ONTOHPLC_PEAKAREA].hasValue.hasNumericalValue / internal_standard_peak_area * internal_standard_run_conc_moleperlitre / dct_response_factor[pt]
            except KeyError:
                raise Exception("ResponseFactor of Species <%s> is not presented in the HPLCMethod <%s>: %s" % (pt, hplc_method.instance_iri, str(hplc_method)))
            concentration = OntoCAPE_Molarity(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                hasValue=OntoCAPE_ScalarValue(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(hplc_report_iri),
                    hasUnitOfMeasure=OM_MOLEPERLITRE,
                    numericalValue=conc_num_val
                )
            )
            # generate phase component
            phase_component = OntoCAPE_PhaseComponent(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                hasProperty=concentration,
                representsOccurenceOf=pt
            )

            chrom_pt = ChromatogramPoint(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                indicatesComponent=phase_component,
                hasPeakArea=dct_points[pt][ONTOHPLC_PEAKAREA],
                atRetentionTime=dct_points[pt][ONTOHPLC_RETENTIONTIME]
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
        output_chemical_iri = initialiseInstanceIRI(getNameSpace(hplc_report_iri), ONTOREACTION_OUTPUTCHEMICAL)
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
        _response = self.performQuery("""SELECT ?sol ?local_report_file ?last_local_modification ?last_upload ?vial
            WHERE{<%s> <%s> ?sol; <%s> ?local_report_file; <%s> ?last_local_modification; <%s> ?last_upload.
            ?sol <%s> ?vial.}""" % (hplc_report_iri, ONTOHPLC_GENERATEDFOR, ONTOHPLC_LOCALFILEPATH, ONTOHPLC_LASTLOCALMODIFIEDAT, ONTOHPLC_LASTUPLOADEDAT, ONTOVAPOURTEC_FILLS))
        if len(_response) > 1:
            raise Exception("Multiple instances of ChemicalSolution identified for HPLCReport <%s>: %s" % (hplc_report_iri, str(_response)))
        elif len(_response) < 1:
            raise Exception("No instance of ChemicalSolution identified for HPLCReport <%s>" % hplc_report_iri)
        else:
            chem_sol_iri = _response[0]['sol']
            chem_sol_vial = _response[0]['vial']
            local_report_file = _response[0]['local_report_file']
            last_local_modified = _response[0]['last_local_modification']
            last_upload = _response[0]['last_upload']

        chemical_solution = ChemicalSolution(
            instance_iri=chem_sol_iri,
            refersToMaterial=output_chemical,
            fills=chem_sol_vial
        )

        # generate hplc report instance
        hplc_report_instance = HPLCReport(
            instance_iri=hplc_report_iri,
            remoteFilePath=remote_hplc_report_path,
            records=list_chrom_pts,
            generatedFor=chemical_solution,
            localFilePath=local_report_file,
            lastLocalModifiedAt=last_local_modified,
            lastUploadedAt=last_upload
        )

        # Remove downloaded temporary file
        os.remove(temp_local_file_path)
        return hplc_report_instance

    def get_chromatogram_point_of_hplc_report(self, hplc_report_iri: str) -> List[ChromatogramPoint]:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = PREFIX_RDF+"""SELECT ?pt ?phase_component ?chemical_species ?conc ?conc_type
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
                       hplc_report_iri, ONTOHPLC_RECORDS, ONTOHPLC_INDICATESCOMPONENT, ONTOHPLC_HASPEAKAREA, ONTOHPLC_ATRETENTIONTIME,
                       ONTOCAPE_REPRESENTSOCCURENCEOF, ONTOCAPE_HASPROPERTY, ONTOCAPE_HASVALUE, ONTOCAPE_HASUNITOFMEASURE, ONTOCAPE_NUMERICALVALUE,
                       OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE
                   )
        response = self.performQuery(query)

        list_chrom_pts = []
        if len(response) != len(dal.get_unique_values_in_list_of_dict(response, 'pt')):
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
                       hplc_report_iri, ONTOHPLC_GENERATEDFOR, ONTOHPLC_REMOTEFILEPATH, ONTOHPLC_LOCALFILEPATH, ONTOHPLC_LASTLOCALMODIFIEDAT, ONTOHPLC_LASTUPLOADEDAT, ONTOVAPOURTEC_FILLS)
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
            remoteFilePath=hplc_report_path,
            localFilePath=local_report_file,
            lastLocalModifiedAt=lastLocalModifiedAt,
            lastUploadedAt=lastUploadedAt,
            records=self.get_chromatogram_point_of_hplc_report(hplc_report_iri),
            generatedFor=ChemicalSolution(
                instance_iri=chem_sol_iri,
                refersToMaterial=self.get_ontocape_material(chem_sol_iri, ONTOCAPE_REFERSTOMATERIAL, ONTOREACTION_OUTPUTCHEMICAL)[0],
                fills=chem_sol_vial
            )
        )

        return hplc_report_instance

    def get_hplc_job_given_hplc_report_iri(self, hplc_report_iri: str) -> HPLCJob:
        hplc_report_instance = self.get_existing_hplc_report(trimIRI(hplc_report_iri))
        return self.get_hplc_job_given_hplc_report_instance(hplc_report_instance)

    def get_hplc_job_given_hplc_report_instance(self, hplc_report_instance: HPLCReport) -> HPLCJob:
        query = """SELECT ?hplc_job ?rxn_exp WHERE {?hplc_job <%s> <%s>; <%s> ?rxn_exp.}""" % (
            ONTOHPLC_HASREPORT, hplc_report_instance.instance_iri, ONTOHPLC_CHARACTERISES)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple instances of HPLCJob/ReactionExperiment is associated with the given HPLCReport <%s>: %s" % (
                hplc_report_instance.instance_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No records of HPLCJob/ReactionExperiment were found with the given HPLCReport instance <%s>" % hplc_report_instance.instance_iri)
        else:
            hplc_job_iri = response[0]['hplc_job']
            rxn_exp_iri = response[0]['rxn_exp']

        hplc_method_instance = self.get_hplc_method_given_hplc_report(hplc_report_instance.instance_iri)
        hplc_job_instance = HPLCJob(
            instance_iri=hplc_job_iri,
            hasReport=hplc_report_instance,
            characterises=self.getReactionExperiment(rxn_exp_iri)[0],
            usesMethod=hplc_method_instance
        )
        return hplc_job_instance

    def get_hplc_job(self, hplc_job_iri: str) -> HPLCJob:
        hplc_job_iri = trimIRI(hplc_job_iri)
        query = """SELECT ?hplc_report ?rxn_exp ?hplc_method WHERE {<%s> <%s> ?hplc_report; <%s> ?rxn_exp; <%s> ?hplc_method.}""" % (
            hplc_job_iri, ONTOHPLC_HASREPORT, ONTOHPLC_CHARACTERISES, ONTOHPLC_USESMETHOD)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple instances of HPLCReport/ReactionExperiment/HPLCMethod is associated with the given HPLCJob <%s>: %s" % (
                hplc_job_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No records of HPLCJob/ReactionExperiment/HPLCMethod were found with the given HPLCJob instance <%s>" % hplc_job_iri)
        else:
            hplc_report_iri = response[0]['hplc_report']
            rxn_exp_iri = response[0]['rxn_exp']
            hplc_method_iri = response[0]['hplc_method']

        hplc_job_instance = HPLCJob(
            instance_iri=hplc_job_iri,
            hasReport=self.get_existing_hplc_report(hplc_report_iri),
            characterises=self.getReactionExperiment(rxn_exp_iri)[0],
            usesMethod=self.get_hplc_method(hplc_method_iri)
        )
        return hplc_job_instance

    def download_remote_raw_hplc_report(self, remote_file_path, downloaded_file_path):
        self.downloadFile(remote_file_path, downloaded_file_path)
        logger.info("Remote raw HPLC report <%s> was successfully downloaded to: %s" % (remote_file_path, downloaded_file_path))

    def connect_hplc_report_with_chemical_solution(self, hplc_report_iri: str, chemical_solution_iri: str):
        hplc_report_iri = trimIRI(hplc_report_iri)
        chemical_solution_iri = trimIRI(chemical_solution_iri)
        update = """INSERT DATA {<%s> <%s> <%s>.}""" % (hplc_report_iri, ONTOHPLC_GENERATEDFOR, chemical_solution_iri)
        self.performUpdate(update)
        logger.info("HPLCReport <%s> is connected to ChemicalSolution <%s>" % (hplc_report_iri, chemical_solution_iri))

    def get_remote_hplc_report_path_given_local_file(self, hplc_digital_twin: str, hplc_local_file: str) -> str:
        hplc_digital_twin = trimIRI(hplc_digital_twin)
        query = PREFIX_XSD+"""SELECT ?remote_path WHERE {<%s> <%s> ?hplc_report. ?hplc_report <%s> "%s"^^xsd:string; <%s> ?remote_path.}""" % (
            hplc_digital_twin, ONTOHPLC_HASPASTREPORT, ONTOHPLC_LOCALFILEPATH, hplc_local_file, ONTOHPLC_REMOTEFILEPATH)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of HPLCReport remote path identified for local file '%s' of HPLC <%s>: %s" % (
                hplc_local_file, hplc_digital_twin, str(response)))
        elif len(response) < 1:
            raise Exception("No record of HPLCReport remote path identified for local file '%s' of HPLC <%s>" % (hplc_local_file, hplc_digital_twin))
        else:
            return response[0]['remote_path']

    def collect_triples_for_performance_indicators(self, lst_performance_indicator: List[PerformanceIndicator], g: Graph) -> Graph:
        for pi in lst_performance_indicator:
            g = pi.create_instance_for_kg(g)
        return g

    def collect_triples_for_chromatogram_point(self, chrom_pts: List[ChromatogramPoint], hplc_report_iri: str, g: Graph) -> Graph:
        for pt in chrom_pts:
            g = pt.create_instance_for_kg(g)
            g.add((URIRef(hplc_report_iri), URIRef(ONTOHPLC_RECORDS), URIRef(pt.instance_iri)))
        return g

    def collect_triples_for_output_chemical_of_chem_sol(self, chemical_solution: ChemicalSolution, rxn_exp_iri: str, g: Graph):
        # NOTE we do NOT call create_instance_for_kg for chemical_solution here
        # NOTE as the triples about the chemical_solution itself (and vial) should already be in the KG
        # <chemical_solution> <refersToMaterial> <output_chemical>
        g.add((URIRef(chemical_solution.instance_iri), URIRef(ONTOCAPE_REFERSTOMATERIAL), URIRef(chemical_solution.refersToMaterial.instance_iri)))
        # Also add triples related to the OutputChemical
        g = chemical_solution.refersToMaterial.create_instance_for_kg(g)
        # <rxn_exp_iri> <hasOutputChemical> <output_chemical>
        g.add((URIRef(rxn_exp_iri), URIRef(ONTOREACTION_HASOUTPUTCHEMICAL), URIRef(chemical_solution.refersToMaterial.instance_iri)))
        return g

    def update_vapourtec_autosampler_liquid_level_millilitre(self, level_change_of_site: Dict[str, float], for_consumption: bool):
        delete_clause = []
        insert_clause = []
        where_clause = []
        i = 0
        for site in level_change_of_site:
            i += 1
            site = trimIRI(site)
            delete_clause.append("""?measure_%s <%s> ?liquid_level_%s .""" % (
                str(i), OM_HASNUMERICALVALUE, str(i)
            ))
            insert_clause.append("""?measure_%s <%s> ?liquid_level_%s_updated .""" % (
                str(i), OM_HASNUMERICALVALUE, str(i)
            ))
            where_clause.append("""<%s> <%s>/<%s>/<%s> ?measure_%s . ?measure_%s <%s> ?liquid_level_%s . BIND (?liquid_level_%s %s %f AS ?liquid_level_%s_updated).""" % (
                site, ONTOVAPOURTEC_HOLDS, ONTOVAPOURTEC_HASFILLLEVEL, OM_HASVALUE, str(i), str(i), OM_HASNUMERICALVALUE, str(i), str(i),
                '-' if for_consumption else '+', level_change_of_site[site], str(i)
            ))

        update = """DELETE {""" + ' '.join(delete_clause) + """} INSERT {""" + ' '.join(insert_clause) + """} WHERE {""" + ' '.join(where_clause) + """}"""
        self.performUpdate(update)

    def create_chemical_solution_for_reaction_outlet(self, autosampler_site_iri: str, amount_of_chemical_solution: float):
        g = Graph()
        autosampler_site_iri = trimIRI(autosampler_site_iri)
        chemical_solution_iri = initialiseInstanceIRI(getNameSpace(autosampler_site_iri), ONTOLAB_CHEMICALSOLUTION)
        g.add((URIRef(chemical_solution_iri), RDF.type, URIRef(ONTOLAB_CHEMICALSOLUTION)))
        update = PREFIX_RDF + """INSERT {<%s> rdf:type <%s>; <%s> ?vial. ?vial <%s> <%s>. } WHERE {<%s> <%s> ?vial.}""" % (
            chemical_solution_iri, ONTOLAB_CHEMICALSOLUTION, ONTOVAPOURTEC_FILLS, ONTOVAPOURTEC_ISFILLEDWITH, chemical_solution_iri,
            autosampler_site_iri, ONTOVAPOURTEC_HOLDS
        )
        self.performUpdate(update)
        self.update_vapourtec_autosampler_liquid_level_millilitre(
            {autosampler_site_iri:amount_of_chemical_solution}, for_consumption=False
        )
        return g

    def release_vapourtec_rs400_settings(self, vapourtec_rs400_iri: str):
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        update = """DELETE {?hardware <%s> ?settings. ?settings <%s> ?hardware.} WHERE {<%s> <%s>* ?hardware. ?hardware <%s> ?settings.}""" % (
                ONTOLAB_ISSPECIFIEDBY, ONTOLAB_SPECIFIES, vapourtec_rs400_iri, SAREF_CONSISTSOF, ONTOLAB_ISSPECIFIEDBY
            )
        self.performUpdate(update)

    def upload_vapourtec_input_file_to_kg(self, vapourtec_digital_twin, local_file_path, remote_file_subdir):
        try:
            remote_file_path, timestamp_upload = self.uploadFile(local_file_path, remote_file_subdir)
            logger.info("Vapourtec input file (%s) was uploaded to fileserver <%s> at %f with remote file path at: %s " % (
                    local_file_path, self.fs_url, timestamp_upload, remote_file_path))
        except Exception as e:
            logger.error(e)
            raise Exception("Vapourtec input file (%s) upload failed with exception %s" % (local_file_path, str(e)))

        vapourtec_input_file_iri = initialiseInstanceIRI(getNameSpace(vapourtec_digital_twin), ONTOVAPOURTEC_VAPOURTECINPUTFILE)
        logger.info("The initialised VapourtecInputFile IRI is: <%s>" % (vapourtec_input_file_iri))

        update = PREFIX_XSD + """INSERT DATA {<%s> <%s> "%s"^^xsd:anyURI; <%s> "%s"^^xsd:string; <%s> %f; <%s> %f.}""" % (
            vapourtec_input_file_iri, ONTOVAPOURTEC_REMOTEFILEPATH, remote_file_path,
            ONTOVAPOURTEC_LOCALFILEPATH, local_file_path, ONTOVAPOURTEC_LASTLOCALMODIFIEDAT, os.path.getmtime(local_file_path),
            ONTOVAPOURTEC_LASTUPLOADEDAT, timestamp_upload
        )
        self.performUpdate(update)

        return vapourtec_input_file_iri

    def get_vapourtec_input_file(self, vapourtec_input_file_iri) -> VapourtecInputFile:
        vapourtec_input_file_iri = trimIRI(vapourtec_input_file_iri)
        query = """SELECT ?lastLocalModifiedAt ?lastUploadedAt ?localFilePath ?remoteFilePath 
                WHERE {<%s> <%s> ?lastLocalModifiedAt; <%s> ?lastUploadedAt; <%s> ?localFilePath; <%s> ?remoteFilePath.}""" % (
                    vapourtec_input_file_iri, ONTOVAPOURTEC_LASTLOCALMODIFIEDAT, ONTOVAPOURTEC_LASTUPLOADEDAT, ONTOVAPOURTEC_LOCALFILEPATH, ONTOVAPOURTEC_REMOTEFILEPATH
                )

        response = self.performQuery(query)
        if (len(response) > 1):
            raise Exception("VapourtecInputFile <%s> should only have one set of information, retrieved: " % (vapourtec_input_file_iri, str(response)))

        vapourtec_input_file = VapourtecInputFile(instance_iri=vapourtec_input_file_iri, **response[0])
        return vapourtec_input_file

    def get_hplc_given_vapourtec_rs400(self, vapourtec_rs400_iri: str) -> HPLC:
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        query = """SELECT ?hplc ?hplc_manufacturer ?lab ?hplc_power_supply ?is_managed_by ?report_extension
                WHERE {<%s> ^<%s>/<%s> ?hplc. ?hplc a <%s>; <%s> ?hplc_manufacturer; <%s> ?lab; <%s> ?hplc_power_supply; <%s> ?report_extension.
                OPTIONAL{?hplc <%s> ?is_managed_by}}""" % (
            vapourtec_rs400_iri, SAREF_CONSISTSOF, SAREF_CONSISTSOF, ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY, DBPEDIA_MANUFACTURER, ONTOLAB_ISCONTAINEDIN, ONTOLAB_HASPOWERSUPPLY,
            ONTOHPLC_REPORTEXTENSION, ONTOLAB_ISMANAGEDBY
        )
        response = self.performQuery(query)
        if (len(response) > 1):
            # NOTE here we assume one HPLC module has only ONE manufacturer, locates in only ONE laboratory, also has only ONE type of power supply
            # NOTE this might not hold universally, but we will simplify for the moment
            raise NotImplementedError("Not yet supported - VapourtecRS400 <%s> is associated with multiple HPLC: %s" % (vapourtec_rs400_iri, str(response)))
        elif (len(response) < 1):
            raise Exception("No HPLC is identified connected with VapourtecRS400 <%s> when querying: %s" % (vapourtec_rs400_iri, query))

        res = response[0]
        hplc = HPLC(
            instance_iri=res['hplc'],
            manufacturer=res['hplc_manufacturer'],
            isContainedIn=res['lab'],
            hasPowerSupply=res['hplc_power_supply'],
            isManagedBy=res['is_managed_by'] if 'is_managed_by' in res else None,
            reportExtension=res['report_extension'],
            # hasJob=, # TODO add support
            # hasPastReport=, # TODO add support
        )
        return hplc

    def detect_new_hplc_report_from_hplc_derivation(self, hplc_derivation_iri: str):
        hplc_derivation_iri = trimIRI(hplc_derivation_iri)
        query = """SELECT ?hplc_report WHERE {?hplc_job <%s> <%s>. ?hplc_job <%s> ?hplc_report.}""" % (
            ONTODERIVATION_BELONGSTO, hplc_derivation_iri, ONTOHPLC_HASREPORT
        )
        response = self.performQuery(query)
        if (len(response) > 1):
            # NOTE here we assume one HPLC Derivation has ONLY ONE HPLCJob and thus ONLY ONE HPLCReport
            # NOTE this might not hold universally, but we will simplify for the moment
            raise NotImplementedError("Not yet supported - HPLC Derivation <%s> is associated with multiple HPLCJob/HPLCReport: %s" % (hplc_derivation_iri, str(response)))
        elif (len(response) < 1):
            logger.info("No HPLCJob/HPLCReport identified yet for HPLC Derivation <%s>." % (hplc_derivation_iri))
            return None
        else:
            return response[0]['hplc_report']

    def check_if_triple_exist(self, s, p, o) -> bool:
        query = """ASK {<%s> <%s> <%s>.}""" % (trimIRI(s), trimIRI(p), trimIRI(o))
        response = self.performQuery(query)
        return response[0]['ASK']
