# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from builtins import Exception
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

    def getDoEInstanceIRI(self, doe_instance: DesignOfExperiment) -> DesignOfExperiment:
        """
            This method retrieves the instance of OntoDoE:DesignOfExperiment given instance of OntoDoE:Strategy, OntoDoE:Domain, OntoDoE:SystemResponse, and OntoDoE:HistoricalData.

            Arguments:
                doe_instance - instance of dataclass OntoDoE.DesignOfExperiment
        """

        # Prepare query string, start with strategy and domain, then iterate over a list of system responses, finally historical data
        query = """SELECT ?doe_instance \
                WHERE { \
                ?doe_instance <%s> <%s> ; \
                    <%s> <%s> ; """ % (ONTODOE_USESSTRATEGY, doe_instance.usesStrategy.instance_iri, ONTODOE_HASDOMAIN, doe_instance.hasDomain.instance_iri)

        for sysres in doe_instance.hasSystemResponse:
            query = query + """<%s> <%s> ; """ % (ONTODOE_HASSYSTEMRESPONSE, sysres.instance_iri)
        query = query + """<%s> <%s> . }""" % (ONTODOE_UTILISESHISTORICALDATA, doe_instance.utilisesHistoricalData.instance_iri)
        # Perform query
        response = self.performQuery(query)
        if (len(response) == 0 ):
            raise Exception("""Unable to identify the OntoDoE:DesignOfExperiment instance given input: \
                OntoDoE:Strategy <%s>; \
                OntoDoE:Domain <%s>; \
                OntoDoE:SystemResponse <%s>; \
                OntoDoE:HistoricalData <%s>.""" % (doe_instance.usesStrategy.instance_iri, doe_instance.hasDomain.instance_iri, ">, <".join([sysres.instance_iri for sysres in doe_instance.hasSystemResponse]), doe_instance.utilisesHistoricalData.instance_iri))
        elif (len(response) > 1):
            raise Exception("""Unable to uniquely identify the OntoDoE:DesignOfExperiment instance given input: \
                OntoDoE:Strategy <%s>; \
                OntoDoE:Domain <%s>; \
                OntoDoE:SystemResponse <%s>; \
                OntoDoE:HistoricalData <%s>. \
                The list of identified OntoDoE:DesignOfExperiment instances are: <%s>.""" % (doe_instance.usesStrategy.instance_iri, doe_instance.hasDomain.instance_iri, ">, <".join([sysres.instance_iri for sysres in doe_instance.hasSystemResponse]), doe_instance.utilisesHistoricalData.instance_iri, ">, <".join([list(r.values())[0] for r in response])))
        doe_instance.__dict__.update(instance_iri=response[0]['doe_instance'])
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
            list_exp.append(
                ReactionExperiment(
                    instance_iri=exp_iri,
                    hasReactionCondition=self.getExpReactionCondition(exp_iri),
                    hasPerformanceIndicator=self.getExpPerformanceIndicator(exp_iri)
                    # TODO add support for parsing InputChemical and OutputChemical
                )
            )
        return list_exp

    def get_input_chemical_of_rxn_exp(self, rxnexp_iri: str) -> List[InputChemical]:
        rxnexp_iri = trimIRI(rxnexp_iri)
        return self.get_ontocape_material(rxnexp_iri, ONTORXN_HASINPUTCHEMICAL)

    def get_ontocape_material(self, subject_iri, predicate_iri) -> List[OntoCAPE_Material]:
        subject_iri = trimIRI(subject_iri)
        predicate_iri = trimIRI(predicate_iri)

        query = PREFIX_RDF + \
                """
                SELECT ?ontocape_material ?single_phase ?state_of_aggregation ?composition ?phase_component ?chemical_species ?phase_component_concentration ?concentration_type ?value ?unit ?num_val
                WHERE {
                    <%s> <%s> ?ontocape_material .
                    ?ontocape_material <%s> ?single_phase .
                    ?single_phase rdf:type <%s> .
                    ?single_phase <%s> ?state_of_aggregation; <%s> ?composition; <%s> ?phase_component .
                    ?composition <%s> ?phase_component_concentration .
                    ?phase_component <%s> ?chemical_species .
                    ?phase_component <%s> ?phase_component_concentration .
                    ?phase_component_concentration rdf:type ?concentration_type; <%s> ?value .
                    ?value <%s> ?unit; <%s> ?num_val.
                }
                """ % (
                    subject_iri, predicate_iri, ONTOCAPE_THERMODYNAMICBEHAVIOR, ONTOCAPE_SINGLEPHASE,
                    ONTOCAPE_HASSTATEOFAGGREGATION, ONTOCAPE_HAS_COMPOSITION, ONTOCAPE_ISCOMPOSEDOFSUBSYSTEM,
                    ONTOCAPE_COMPRISESDIRECTLY, ONTOCAPE_REPRESENTSOCCURENCEOF, ONTOCAPE_HASPROPERTY,
                    ONTOCAPE_HASVALUE, ONTOCAPE_HASUNITOFMEASURE, ONTOCAPE_NUMERICALVALUE
                )

        response = self.performQuery(query)

        lst_ontocape_material = []

        # generate different list for each OntoCAPE:Material
        unique_ontocape_material_iri = self.get_unique_values_in_list_of_dict(response, 'ontocape_material')
        list_list_ontocape_material = []
        for iri in unique_ontocape_material_iri:
            list_list_ontocape_material.append([res for res in response if iri == res['ontocape_material']])

        for list_om in list_list_ontocape_material:
            ontocape_material_iri = self.get_unique_values_in_list_of_dict(list_om, 'ontocape_material')[0] # here we are sure this is the unique value of OntoCAPE:Material

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

            input_chemical = InputChemical(instance_iri=ontocape_material_iri,thermodynamicBehaviour=single_phase)

            lst_ontocape_material.append(input_chemical)

        return lst_ontocape_material

    def get_all_autosampler_with_fill(self) -> List[AutoSampler]:
        query = PREFIX_RDF + \
                """
                SELECT ?autosampler ?site ?loc ?vial ?fill_level ?fill_level_om_value ?fill_level_unit ?fill_level_num_val
                ?max_level ?max_level_om_value ?max_level_unit ?max_level_num_val ?chemical_solution
                WHERE {
                    ?autosampler rdf:type <%s>.
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
                    ONTOVAPOURTEC_AUTOSAMPLER, ONTOVAPOURTEC_HASSITE, ONTOVAPOURTEC_HOLDS, ONTOVAPOURTEC_LOCATIONID,
                    ONTOVAPOURTEC_HASFILLLEVEL, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    ONTOVAPOURTEC_HASMAXLEVEL, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    ONTOVAPOURTEC_ISFILLEDWITH
                )

        response = self.performQuery(query)

        unique_autosampler_list = self.get_unique_values_in_list_of_dict(response, 'autosampler')
        logger.debug("The list of all available OntoVapourtec:AutoSampler in the knowledge graph (%s): %s" % (self.kg_client.getQueryEndpoint(), str(unique_autosampler_list)))
        list_autosampler = []
        for specific_autosampler in unique_autosampler_list:
            info_of_specific_autosampler = self.get_sublist_in_list_of_dict_matching_key_value(response, 'autosampler', specific_autosampler)
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
    def get_dt_of_preferred_hardware(self, list_equip_settings: ReactionExperiment):
        # query if there's suitable hardware
        # first step: query if suitable chemicals given the experiment --> does the vial hold the chemicals?
        
        pass

    # \item append the OntoLab:EquipmentSettings to the digital twin, label it with triples <OntoLab:LabEquipment OntoLab:hasPendingEquipSettings OntoLab:EquipmentSettings>
    def enqueue_settings_to_pending(self, list_equip_settings: List[EquipmentSettings], list_equip_digital_twin: List[LabEquipment]):
        # add settings to pending list
        pass

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
