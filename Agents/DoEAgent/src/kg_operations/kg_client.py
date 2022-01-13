# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from rdflib import Graph, URIRef, Namespace, Literal, BNode
from rdflib.namespace import RDF
import pandas as pd
import collections
import json
import uuid
import os

# get the jpsBaseLibGW instance from the jpsSingletons module
from kg_operations.gateway import jpsBaseLibGW, jpsBaseLib_view

from data_model import *
from conf import *

class KGClient:
    def __init__(self, triple_store_server, triple_store_repo, query_endpoint, update_endpoint, kg_user, kg_password) -> None:
        self.triple_store_server = triple_store_server
        self.triple_store_repo = triple_store_repo
        self.kr_client = jpsBaseLib_view.KnowledgeRepository()
        if kg_user is not None:
            self.kg_client = jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user, kg_password)
        else:
            self.kg_client = jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)

    def updateNewExperimentInKG(self, doe: DesignOfExperiment, newExp: NewExperiment):
        """
            This method is used to populate the suggested new experiments back to the knowledge graph.
            It firstly first serialise and upload NewExperiment instance (including ReactionVariation) to the knowledge graph.
            It then replace the link between the OntoDoE:DesignOfExperiment instance and the old OntoDoE:NewExperiment instance with the new created OntoDoE:NewExperiment instance.

            Arguments:
                doe - instance of dataclass OntoDoE.DesignOfExperiment
                newExp - instance of dataclass OntoDoE.NewExperiment
        """
        # (1) first serialise and upload NewExperiment instance (including ReactionVariation) to KG
        # Generate a file path that is used to store the created OntoRxn:ReactionVariation instance
        filePath = f'{str(uuid.uuid4())}.xml'
        # Serialise the created OntoDoE:NewExperiment instance (including OntoRxn:ReactionVariation) as a XML file
        # All information should already be prepared and added to the instance
        # Method createInstanceForKG will write all information to rdflib Graph on-the-fly
        g = Graph()
        g = newExp.createInstanceForKG(g)
        g.serialize(filePath, format='xml')
        # Upload the created OntoDoE:NewExperiment instance to knowledge graph
        self.uploadOntology(filePath)
        # Delete generated XML file
        os.remove(filePath)

        # (2) replace connection between OntoDoE:DesignOfExperiment with OntoDoE:NewExperiment
        # Construct SPARQL Update string
        # delete existing <DoE> <proposesNewExperiment> <newExp_old>
        # add <DoE> <proposesNewExperiment> <newExp>
        update = """DELETE {<%s> <%s> ?newexp .} \
                    INSERT {<%s> <%s> <%s> .} \
                    WHERE {<%s> <%s> ?newexp .}""" % (
                        doe.instance_iri, ONTODOE_PROPOSESNEWEXPERIMENT,
                        doe.instance_iri, ONTODOE_PROPOSESNEWEXPERIMENT, newExp.instance_iri,
                        doe.instance_iri, ONTODOE_PROPOSESNEWEXPERIMENT)

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
        tsemo_instance = TSEMO(tsemo_iri,**response[0])
        return tsemo_instance

    def checkInstanceClass(self, instance, instance_class):
        """
            This method checks if the given instance is instantiated from the given instance class.

            Arguments:
                instance - IRI of an instance
                instance_class - IRI of the instance class to be checked against
        """
        # Delete "<" and ">" around the IRI
        instance = trimIRI(instance)
        instance_class = trimIRI(instance_class)

        # Prepare query string, ignore owl:Thing and owl:NamedIndividual
        query = PREFIX_RDFS + \
                PREFIX_RDF + \
                PREFIX_XSD + \
                PREFIX_OWL + \
                """SELECT ?result \
                WHERE { <%s> rdf:type ?type . \
                FILTER(?type != owl:Thing && ?type != owl:NamedIndividual) . \
                BIND(xsd:boolean(if(?type = <%s>, "true", "false")) as ?result)  \
                }""" % (instance, instance_class)
        
        # Perform query
        response = self.performQuery(query)
        res = [list(r.values())[0] for r in response]
        if res[0] == 'true':
            return True
        else:
            return False
    
    def performQuery(self, query):
        """
            This function performs query to knowledge graph.

            Arguments:
                query - SPARQL Query string
        """
        response = self.kg_client.execute(query)
        return json.loads(response)

    def performUpdate(self, update):
        """
            This function performs SPARQL Update to knowledge graph.

            Arguments:
                update - SPARQL Update string
        """
        self.kg_client.executeUpdate(update)

    def uploadOntology(self, filePath):
        """
            This function uploads ontology to knowledge graph.

            Arguments:
                filePath - the file path of ontology to be uploaded
        """
        self.kr_client.uploadOntology(self.triple_store_server, self.triple_store_repo, filePath)
