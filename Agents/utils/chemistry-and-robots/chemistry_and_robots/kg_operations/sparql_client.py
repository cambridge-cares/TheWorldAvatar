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

    def get_ontocape_single_phase(self, ontocape_material_iri: str):# -> OntoCAPE_SinglePhase:
        ontocape_material_iri = trimIRI(ontocape_material_iri)

        query = PREFIX_RDF + \
                """
                SELECT ?single_phase ?state_of_aggregation ?composition ?phase_component ?chemical_species ?phase_component_concentration ?concentration_type ?value ?unit ?num_val
                WHERE {
                    <%s> <%s> ?single_phase .
                    ?single_phase <%s> ?state_of_aggregation; <%s> ?composition; <%s> ?phase_component .
                    ?composition <%s> ?phase_component_concentration .
                    ?phase_component <%s> ?chemical_species .
                    ?phase_component <%s> ?phase_component_concentration .
                    ?phase_component_concentration rdf:type ?concentration_type; <%s> ?value .
                    ?value <%s> ?unit; <%s> ?num_val.
                }
                """ % (
                    ontocape_material_iri, ONTOCAPE_THERMODYNAMICBEHAVIOR,
                    ONTOCAPE_HASSTATEOFAGGREGATION, ONTOCAPE_HAS_COMPOSITION, ONTOCAPE_ISCOMPOSEDOFSUBSYSTEM,
                    ONTOCAPE_COMPRISESDIRECTLY, ONTOCAPE_REPRESENTSOCCURENCEOF, ONTOCAPE_HASPROPERTY,
                    ONTOCAPE_HASVALUE, ONTOCAPE_HASUNITOFMEASURE, ONTOCAPE_NUMERICALVALUE
                )

        response = self.performQuery(query)

        # TODO firstly, validate that the list of responses are only referring to one instance of OntoCAPE_SinglePhase and one instance of composition, otherwise raise an Exception
        # TODO at the same time, assign composition_iri and single_phase_iri

        # TODO secondly, get a list of OntoCAPE_PhaseComponent to be added to the OntoCAPE_SinglePhase instance
        list_phase_component = []
        list_phase_component_concentration = []
        for res in response:
            if 'concentration_type' in res:
                if res['concentration_type'] == OntoCAPE_Molarity.__fields__['clz'].default:
                    concentration = OntoCAPE_Molarity(instance_iri=res['phase_component_concentration'],hasValue=OntoCAPE_ScalarValue(instance_iri=res['value'],numerivalValue=res['num_val'],hasUnitOfMeasure=res['unit']))
                else:
                    # TODO add support for other type of OntoCAPE_PhaseComponentConcentration
                    pass
                list_phase_component_concentration.append(concentration)
            else:
                raise Exception("Concentration is not defined for")

            phase_component = OntoCAPE_PhaseComponent(instance_iri=res['phase_component'],representsOccurenceOf=res['chemical_species'],hasProperty=concentration)
            list_phase_component.append(phase_component)

        composition = OntoCAPE_Composition(instance_iri=composition_iri,comprisesDirectly=list_phase_component_concentration)
        single_phase = OntoCAPE_SinglePhase(instance_iri=single_phase_iri,has_composition=composition,isComposedOfSubsystem=list_phase_component,representsThermodynamicBehaviorOf=ontocape_material_iri)
        # [
            # {
            #     "chemical_species":"http://www.theworldavatar.com/kb/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b",
            #     "unit":"http://www.ontology-of-units-of-measure.org/resource/om-2/molePerLitre",
            #     "phase_component_concentration":"https://www.example.com/triplestore/ontorxn/SinglePhase/PhaseComponent_1_Property_1",
            #     "composition":"https://www.example.com/triplestore/ontorxn/SinglePhase/Composition_1",
            #     "single_phase":"https://www.example.com/triplestore/ontorxn/SinglePhase/Phase_1",
            #     "num_val":"0.5",
            #     "state_of_aggregation":"http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#liquid",
            #     "phase_component":"https://www.example.com/triplestore/ontorxn/SinglePhase/PhaseComponent_1",
            #     "value":"https://www.example.com/triplestore/ontorxn/SinglePhase/PhaseComponent_1_Property_1_ScalarValue_1"
            # },
            # {
            #     "chemical_species":"http://www.theworldavatar.com/kb/ontospecies/Species_0401f93b-b62d-488e-ba1f-7d5c37e365cb",
            #     "unit":"http://www.ontology-of-units-of-measure.org/resource/om-2/molePerLitre",
            #     "phase_component_concentration":"https://www.example.com/triplestore/ontorxn/SinglePhase/PhaseComponent_6_Property_1",
            #     "composition":"https://www.example.com/triplestore/ontorxn/SinglePhase/Composition_1",
            #     "single_phase":"https://www.example.com/triplestore/ontorxn/SinglePhase/Phase_1",
            #     "num_val":"18.1",
            #     "state_of_aggregation":"http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#liquid",
            #     "phase_component":"https://www.example.com/triplestore/ontorxn/SinglePhase/PhaseComponent_6",
            #     "value":"https://www.example.com/triplestore/ontorxn/SinglePhase/PhaseComponent_6_Property_1_ScalarValue_1"
            # }
            # ]
        return response

    def get_input_chemical_of_rxn_exp(self, rxnexp_iri: str) -> List[InputChemical]:
        rxnexp_iri = trimIRI(rxnexp_iri)

        query = """
                """

        # input_chemical = InputChemical(
        #     instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
        #     namespace_for_init=getNameSpace(rxnexp_iri),
        #     thermodynamicBehaviour=OntoCAPE_SinglePhase(
        #         instance_iri=,
        #         namespace_for_init=,
        #         hasStateOfAggregation=OntoCAPE_liquid,
        #         representsThermodynamicBehaviorOf=input_chemical,
        #         isComposedOfSubsystem=,
        #         has_composition=,
        #     ),
        # )

    #         hasStateOfAggregation: OntoCAPE_StateOfAggregation
    # representsThermodynamicBehaviorOf: OntoCAPE_Material
    # isComposedOfSubsystem: List[OntoCAPE_PhaseComponent]
    # has_composition: OntoCAPE_Composition
        # prefix ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
        # prefix ontodoe: <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#>
        # prefix ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        # prefix ontocapeupp: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        # prefix ontocapepha: <http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
        # prefix ontocaperxn: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
        # prefix ontocapemat: <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>
        # prefix ontorxn: <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#>
        # select ?exp ?input_chemical ?species ?val ?unit
        # where {
        #   ?exp rdf:type ontorxn:ReactionExperiment .
        #   ?exp ontorxn:hasInputChemical ?input_chemical .
        #   ?input_chemical ontocapemat:thermodynamicBehavior ?single_phase .
        #   ?single_phase ontocapeupp:isComposedOfSubsystem ?phase_component .
        #   ?single_phase ontocapepha:has_composition/ontocapeupp:comprisesDirectly ?phase_component_property .
        #   ?phase_component ontocapeupp:hasProperty ?phase_component_property .
        #   ?phase_component_property ontocapeupp:hasValue ?value .
        #   ?value ontocapeupp:hasUnitOfMeasure ?unit;
        #          ontocapeupp:numericalValue ?val.
        #   ?phase_component ontocapepha:representsOccurranceOf ?species .
        # }
        pass

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
