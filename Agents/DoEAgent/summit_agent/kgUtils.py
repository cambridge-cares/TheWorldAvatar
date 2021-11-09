# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
from .jpsSingletons import jpsBaseLibGW, jpsBaseLib_view
from .resources.parameter import *
from .resources.doeagent_properties import *
from functools import reduce
import pandas as pd
import json
from rdflib import Graph, URIRef, Namespace, Literal, BNode
from rdflib.namespace import RDF
import uuid

def createOntoDoENewExperimentIRI(endpoint, doe_instance, new_exp_iri_list):
    """
        This method creates the OntoDoE:NewExperiment instance given the OntoDoE:DesignOfExperiment instance and OntoRxn:ReactionExperiment/OntoRxn:ReactionVariation instances.

        Arguments:
            endpoint - SPARQL Query endpoint
            doe_instance - IRI of an instance of OntoDoE:DesignOfExperiment
            new_exp_iri_list - a list of IRI of the instances of OntoRxn:ReactionExperiment or OntoRxn:ReactionVariation
    """
    # Delete "<" and ">" around the IRI
    new_exp_iri_list = trimIRI(new_exp_iri_list)
    # Generate IRI of OntoDoE:NewExperiment instance
    ontodoe_new_exp_iri = getNameSpace(doe_instance) + getShortName(ONTODOE_NEWEXPERIMENT) + '_' + str(uuid.uuid4())

    # Construct SPARQL Update string
    update = PREFIX_RDF + \
             """INSERT DATA { \
             <%s> rdf:type <%s> . \
             <%s> <%s> <%s> . """ % (ontodoe_new_exp_iri, ONTODOE_NEWEXPERIMENT, doe_instance, ONTODOE_PROPOSESNEWEXPERIMENT, ontodoe_new_exp_iri)

    # Add safeguard in case the input new_exp_iri_list is not a list
    if not isinstance(new_exp_iri_list, list):
        new_exp_iri_list = [new_exp_iri_list]
    for new_exp_iri in new_exp_iri_list:
        update = update + """<%s> <%s> <%s> . """ % (ontodoe_new_exp_iri, ONTODOE_REFERSTO, new_exp_iri)
    update = update + """}"""
    # Perform SPARQL Update
    performUpdate(endpoint, update)
    return ontodoe_new_exp_iri

def getDoEInstanceIRI(endpoint, strategy_instance, domain_instance, systemResponse_instances, historicalData_instance):
    """
        This method retrieves the instance of OntoDoE:DesignOfExperiment given instance of OntoDoE:Strategy, OntoDoE:Domain, OntoDoE:SystemResponse, and OntoDoE:HistoricalData.

        Arguments:
            endpoint - SPARQL Query endpoint
            strategy_instance - instance of OntoDoE:Strategy
            domain_instance - instance of OntoDoE:Domain
            systemResponse_instances - a list of instance of OntoDoE:SystemResponse
            historicalData_instance - instance of OntoDoE:HistoricalData
    """
    # Delete "<" and ">" around the IRI
    historicalData_instance = trimIRI(historicalData_instance)
    # Prepare query string, start with strategy and domain, then iterate over a list of system responses, finally historical data
    query = """SELECT ?doe_instance \
            WHERE { \
            ?doe_instance <%s> <%s> ; \
                <%s> <%s> ; """ % (ONTODOE_USESSTRATEGY, strategy_instance, ONTODOE_HASDOMAIN, domain_instance)

    # Add safeguard in case the input systemResponse_instances is not a list
    if not isinstance(systemResponse_instances, list):
        systemResponse_instances = [systemResponse_instances]
    for sysres in systemResponse_instances:
        query = query + """<%s> <%s> ; """ % (ONTODOE_HASSYSTEMRESPONSE, sysres)
    query = query + """<%s> <%s> . }""" % (ONTODOE_UTILISESHISTORICALDATA, historicalData_instance)
    # Perform query
    response = performQuery(endpoint, query)
    if (len(response) == 0 ):
        raise Exception("""Unable to identify the OntoDoE:DesignOfExperiment instance given input: \
            OntoDoE:Strategy <%s>; \
            OntoDoE:Domain <%s>; \
            OntoDoE:SystemResponse <%s>; \
            OntoDoE:HistoricalData <%s>.""" % (strategy_instance, domain_instance, ">, <".join(systemResponse_instances), historicalData_instance))
    elif (len(response) > 1):
        raise Exception("""Unable to uniquely identify the OntoDoE:DesignOfExperiment instance given input: \
            OntoDoE:Strategy <%s>; \
            OntoDoE:Domain <%s>; \
            OntoDoE:SystemResponse <%s>; \
            OntoDoE:HistoricalData <%s>. \
            The list of identified OntoDoE:DesignOfExperiment instances are: <%s>.""" % (strategy_instance, domain_instance, ">, <".join(systemResponse_instances), historicalData_instance, ">, <".join([list(r.values())[0] for r in response])))
    return response[0]

def getDoEAgentInputs(endpoint, derivation):
    """
        This method retrieves the DoE Agent inputs given the IRI of OntoDerivation:Derivation instance, and map those inputs against the I/O signiture declared in the OntoAgent instance of DoE Agent.
        The inputs are finally structured as a JSON string to be feed into the DoE Agent for suggestions.

        Arguments:
            endpoint - SPARQL Query endpoint
            derivation - instance of OntoDerivation:Derivation
    """
    # Delete "<" and ">" around the IRI
    derivation = trimIRI(derivation)
    # Prepare query string, the "rdf:type+" uses the "property paths" feature provided in SPARQL 1.1 to enable arbitrary length of path matching
    query = PREFIX_RDF + \
            """SELECT DISTINCT ?name ?type ?input \
            WHERE { \
            <%s> <%s> ?operation . \
            ?operation <%s> ?mc . \
            ?mc <%s> ?part . \
            ?part <%s> ?type ; \
                  <%s> ?name . \
            <%s> <%s> ?input . \
            ?input rdf:type+ ?type . \
            }""" % (DOEAGENT_ONTOAGENT_SERVICE, ONTOAGENT_HASOPERATION, ONTOAGENT_HASINPUT, ONTOAGENT_HASMANDATORYPART, ONTOAGENT_HASTYPE, ONTOAGENT_HASNAME, derivation, ONTODERIVATION_ISDERIVEDFROM)
    # Perform query
    response = performQuery(endpoint, query)
    # Construct the inputs as a JSON (dict in python)
    inputs = {}
    for r in response:
        if r['type'] in inputs:
            if isinstance(inputs[r['type']], list):
                inputs[r['type']].append(r['input'])
            else:
                inputs[r['type']] = [inputs[r['type']]]
                inputs[r['type']].append(r['input'])
        else:
            inputs[r['type']] = r['input']
    return {DOEAGENT_INPUT_JSON_KAY: inputs}

def getIndicatesInputChemical(endpoint, rxn_instance, clz, positionalID: int):
    """
        This method retrieves the object property and the instance related to the OntoRxn:InputChemical in an OntoRxn:ReactionExperiment/OntoRxn:ReactionVariation instance.
        This method returns both the object property used to link the reaction condition and OntoRxn:InputChemical instance, also the OntoRxn:InputChemical instance itself.

        Arguments:
            endpoint - SPARQL Query endpoint
            rxn_instance - instance of OntoRxn:ReactionExperiment or OntoRxn:ReactionVariation
            clz - IRI of class of the reaction condition
            positionalID - integer of the positional ID of the reaction condition if exists
    """
    # Delete "<" and ">" around the IRI
    rxn_instance = trimIRI(rxn_instance)
    clz = trimIRI(clz)
    # Perpare and perform different query for cases whether positionalID exists
    if positionalID is not None:
        positionalID = int(positionalID)
        query = PREFIX_RDF + \
                """SELECT DISTINCT ?o ?indicates ?inputChem \
                WHERE { \
                <%s> ?p ?o . \
                ?o rdf:type <%s>; \
                   <%s> %d; \
                   ?indicates ?inputChem . \
                ?inputChem rdf:type <%s> . \
                }""" % (rxn_instance, clz, ONTODOE_POSITIONALID, positionalID, ONTORXN_INPUTCHEMICAL)
        response = performQuery(endpoint, query)
        if (len(response) > 1):
            raise Exception("InputChemical should be uniquely identified within <%s>, given <%s> and unique positionalID %d." % (rxn_instance, clz, positionalID))
        return response[0]
    else:
        query = PREFIX_RDF + \
                """SELECT DISTINCT ?o ?indicates ?inputChem \
                WHERE { \
                <%s> ?p ?o . \
                ?o rdf:type <%s>; \
                   ?indicates ?inputChem . \
                ?inputChem rdf:type <%s> . \
                }""" % (rxn_instance, clz, ONTORXN_INPUTCHEMICAL)
        response = performQuery(endpoint, query)
        if (len(response) > 1):
            raise Exception("InputChemical should be uniquely identified within <%s>, given <%s> when positionalID is unnecessary." % (rxn_instance, clz, positionalID))
        return response[0]

def getQuantityUnit(endpoint, rxn_instance, clz, positionalID: int):
    """
        This method retrieves the om:Unit of an instance of om:Quantity. In OntoRxn:ReactionExperiment/OntoRxn:ReactionVariation, the instance of om:Quantity refers to the reaction conditions or performance indicators.

        Arguments:
            endpoint - SPARQL Query endpoint
            rxn_instance - instance of OntoRxn:ReactionExperiment or OntoRxn:ReactionVariation
            clz - IRI of class of the reaction condition
            positionalID - integer of the positional ID of the reaction condition if exists
    """
    # Delete "<" and ">" around the IRI
    rxn_instance = trimIRI(rxn_instance)
    clz = trimIRI(clz)
    # Perpare and perform different query for cases whether positionalID exists
    if positionalID is not None:
        positionalID = int(positionalID)
        query = PREFIX_RDF + \
                """SELECT DISTINCT ?o ?unit \
                WHERE { \
                <%s> ?p ?o . \
                ?o rdf:type <%s>; \
                   <%s> %d; \
                   <%s> ?v . \
                ?v <%s> ?unit . \
                }""" % (rxn_instance, clz, ONTODOE_POSITIONALID, positionalID, OM_HASVALUE, OM_HASUNIT)
        response = performQuery(endpoint, query)
        if (len(response) > 1):
            raise Exception("Instance of <%s> with a positionalID %d should be uniquely identified within reaction experiment <%s>." % (clz, positionalID, rxn_instance))
        return response[0]['unit']
    else:
        query = PREFIX_RDF + \
                """SELECT DISTINCT ?o ?unit \
                WHERE { \
                <%s> ?p ?o . \
                ?o rdf:type <%s>; \
                   <%s> ?v . \
                ?v <%s> ?unit . \
                }""" % (rxn_instance, clz, OM_HASVALUE, OM_HASUNIT)
        response = performQuery(endpoint, query)
        if (len(response) > 1):
            raise Exception("Instance of <%s> should be uniquely identified within reaction experiment <%s> when positionalID is unnecessary." % (clz, rxn_instance))
        return response[0]['unit']

def getObjectRelationship(endpoint, rxn_instance, clz):
    """
        This method retrieves the object property between an OntoRxn:ReactionExperiment/OntoRxn:ReactionVariation instance and a OntoRxn:ReactionCondition or OntoRxn:PerformanceIndicator class and their subclasses.

        Arguments:
            endpoint - SPARQL Query endpoint
            rxn_instance - instance of OntoRxn:ReactionExperiment or OntoRxn:ReactionVariation
            clz - IRI of class of the reaction condition
    """
    # Delete "<" and ">" around the IRI
    rxn_instance = trimIRI(rxn_instance)
    clz = trimIRI(clz)
    # Prepare query string
    query = PREFIX_RDF + \
            """SELECT DISTINCT ?p \
            WHERE { \
            <%s> ?p ?o . \
            ?o rdf:type <%s> . \
            }""" % (rxn_instance, clz)
    # Perform query
    response = performQuery(endpoint, query)
    res = [list(r.values())[0] for r in response]
    return res

def getNumOfNewExpToGenerate(endpoint, historicalData_instance):
    """
        This method retrieves the number of experiment to be generated by the DoE Agent given an instance of OntoDoE:HistoricalData.

        Arguments:
            endpoint - SPARQL Query endpoint
            historicalData_instance - IRI of instance of OntoDoE:HistoricalData
    """
    # Delete "<" and ">" around the IRI
    historicalData_instance = trimIRI(historicalData_instance)
    # Prepare query string
    query = """SELECT ?numOfExp \
            WHERE {
            <%s> <%s> ?numOfExp . \
            }""" % (historicalData_instance, ONTODOE_NUMOFNEWEXP)
    # Perform query
    response = performQuery(endpoint, query)
    return response[0]

def getFirstInstanceOfExperiment(endpoint, historicalData_instance):
    """
        This method retrieves the first instance of the OntoRxn:ReactionExperiment in the list of OntoRxn:ReactionExperiment instances pointed by the OntoDoE:HistoricalData instance.
        
        Arguments:
            endpoint - SPARQL Query endpoint
            historicalData_instance - IRI of instance of OntoDoE:HistoricalData
    """
    # Delete "<" and ">" around the IRI
    historicalData_instance = trimIRI(historicalData_instance)
    # Prepare query string
    query = PREFIX_RDF + \
            """SELECT ?first_exp \
            WHERE { \
            <%s> <%s> ?first_exp . \
            ?first_exp rdf:type <%s> . \
            } LIMIT 1""" % (historicalData_instance, ONTODOE_REFERSTO, ONTORXN_REACTIONEXPERIMENT)
    # Perform query
    response = performQuery(endpoint, query)
    return response[0]

def constructHistoricalDataTable(endpoint, domain_instance, systemResponse_instances, historicalData_instance):
    """
        This method constructs a table of historical data to be used by the `Summit` package to suggest the next experiments.
        A list of 'continuousVariables' in the OntoDoE:Domain instance and a list of 'systemResponses' are also returned to aid the design of experiment exercise of `Summit` package.
        This method currently only supports continuour variables for optimisation.
        (For package `Summit`, please visit: https://gosummit.readthedocs.io/en/latest/index.html)
        
        Arguments:
            endpoint - SPARQL Query endpoint
            domain_instance - IRI of instance of OntoDoE:Domain
            systemResponse_instances - a list of IRI of instances of OntoDoE:SystemResponse, i.e. even there's only one system response we are interested in, it should be provided in the form of a list, e.g. ['https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1']
            historicalData_instance - IRI of instance of OntoDoE:HistoricalData
    """
    # Delete "<" and ">" around the IRI
    domain_instance = trimIRI(domain_instance)
    historicalData_instance = trimIRI(historicalData_instance)
    
    # Initialise the lists of dict for design variables and system responses to be returned
    list_of_designVariable_dict = []
    list_of_systemResponse_dict = []

    # Initialise the list of dict for historical data that will be turned into pandas.DataFrame
    list_of_hist_data_df = []

    # Retrieves a list of design variables given the input instance of OntoDoE:Domain
    list_of_design_variables = getDesignVariables(endpoint, domain_instance)
    # Iterate over that list
    for var in list_of_design_variables:
        # Prepare data for dictionary of design variable
        # name: getShortName(var), description: getShortName(clz)+_id, lower: query result, upper: query result
        des_var = {}
        des_var['name'] = getShortName(var['var'])
        des_var['description'] = getShortName(var['clz']) + "_" + ( str(var['id'] if 'id' in var else 'Only') )
        des_var['lower'] = var['lower'] if 'lower' in var else None
        des_var['upper'] = var['upper'] if 'upper' in var else None
        des_var['clz'] = var['clz']
        if 'id' in var:
            des_var['id'] = var['id']
        list_of_designVariable_dict.append(des_var)
        # Prepare data for the historical data table, the prepared data will be converted from a dict to a pandas.DataFrame and added to a list
        data = getHistoricalDataOfVariable(endpoint, historicalData_instance, var['var'], var['clz'], var['id'] if 'id' in var else None)
        _to_df = {}
        for k in data[0]:
            _to_df[k] = tuple(d[k] for d in data)
        list_of_hist_data_df.append(pd.DataFrame.from_dict(_to_df))
    
    # Retrieves a list of system responses given the input instances of OntoDoE:SystemResponse
    list_of_system_response = getSystemResponses(endpoint, systemResponse_instances)
    # Iterate over that list
    for res in list_of_system_response:
        # Prepare data for dictionary of system response
        # name: getShortName(res), description: getShortName(clz)+_id, direction: direction from query
        sys_res = {}
        sys_res['name'] = getShortName(res['res'])
        sys_res['description'] = getShortName(res['clz']) + "_" + ( str(res['id'] if 'id' in res else 'Only') )
        sys_res['direction'] = 'maximise' if 'true' in res['maximise'] else "minimise"
        sys_res['clz'] = res['clz']
        if 'id' in res:
            sys_res['id'] = res['id']
        list_of_systemResponse_dict.append(sys_res)
        # Prepare data for the historical data table, the prepared data will be converted from a dict to a pandas.DataFrame and added to a list
        data = getHistoricalDataOfVariable(endpoint, historicalData_instance, res['res'], res['clz'], res['id'] if 'id' in res else None)
        _to_df = {}
        for k in data[0]:
            _to_df[k] = tuple(d[k] for d in data)
        list_of_hist_data_df.append(pd.DataFrame.from_dict(_to_df))
    
    # Merge the list of pandas.DataFrame to one DataFrame, using the IRI of OntoRxn:ReactionExperiment as unique identifier
    historicalData_df = reduce(lambda df1, df2: pd.merge(df1, df2, on='rxnexp'), list_of_hist_data_df)
    return {"continuousVariables": list_of_designVariable_dict}, {"systemResponses": list_of_systemResponse_dict}, historicalData_df

def getDesignVariables(endpoint, domain_instance):
    """
        This methods retrieves all the design variables within the given instance of OntoDoE:Domain.

        Arguments:
            endpoint - SPARQL Query endpoint
            domain_instance - IRI of instance of OntoDoE:Domain
    """
    # Delete "<" and ">" around the IRI
    domain_instance = trimIRI(domain_instance)
    # Prepare query string
    query = """SELECT DISTINCT ?var ?clz ?id ?lower ?upper \
            WHERE { \
                <%s> <%s> ?var . \
                ?var <%s> ?clz . \
                OPTIONAL {?var <%s> ?id . } \
                OPTIONAL {?var <%s> ?lower . } \
                OPTIONAL {?var <%s> ?upper . } \
            }""" % (domain_instance, ONTODOE_HASDESIGNVARIABLE, ONTODOE_REFERSTO, ONTODOE_POSITIONALID, ONTODOE_LOWERLIMIT, ONTODOE_UPPERLIMIT)
    
    # Perform query
    response = performQuery(endpoint, query)
    return response

def getSystemResponses(endpoint, systemResponse_instances):
    """
        This methods retrieves all the system responses given instances of OntoDoE:SystemResponses.

        Arguments:
            endpoint - SPARQL Query endpoint
            systemResponse_instances - a list of OntoDoE:SystemResponse instances
    """
    list_sys = []
    # Add safeguard in case the input systemResponse_instances is not a list
    if not isinstance(systemResponse_instances, list):
        systemResponse_instances = [systemResponse_instances]
    for sys_ins in systemResponse_instances:
        # Delete "<" and ">" around the IRI
        sys_ins = trimIRI(sys_ins)
        # Prepare query string
        query = """SELECT DISTINCT ?clz ?id ?maximise \
                WHERE { \
                <%s> <%s> ?clz . 
                OPTIONAL {<%s> <%s> ?id} \
                OPTIONAL {<%s> <%s> ?maximise} \
                }""" % (sys_ins, ONTODOE_REFERSTO, sys_ins, ONTODOE_POSITIONALID, sys_ins, ONTODOE_MAXIMISE)
        # Perform query
        response = performQuery(endpoint, query)
        response[0]['res'] = sys_ins
        list_sys += response
    return list_sys

def getHistoricalDataOfVariable(endpoint, historicalData_instance, variable_instance, instance_class, positionalID=None):
    """
        This method retrieves a list of information for an instance of om:Quantity across different instances of experiment data pointed by the given instance of OntoDoE:HistoricalData.
        It works for both an instance of OntoDoE:ContinuousVariable as part of an instance of OntoDoE:Domain, and an instance of OntoDoE:SystemResponse.

        Arguments:
            endpoint - SPARQL Query endpoint
            historicalData_instance - IRI of instance of OntoDoE:HistoricalData
            variable_instance - IRI of instance of the om:Quantity of interest
            instance_class - IRI of the class of the variable_instance
            positionalID - integer of the positional ID of the variable_instance if exists
    """
    # Delete "<" and ">" around the IRI
    historicalData_instance = trimIRI(historicalData_instance)
    instance_class = trimIRI(instance_class)

    # Prepare query string depends on if positionalID exists
    if positionalID:
        query = PREFIX_OM + \
                PREFIX_RDF + \
                """SELECT DISTINCT ?rxnexp ?%s \
                WHERE { \
                <%s> <%s> ?rxnexp . \
                ?rxnexp ?p ?o . \
                ?o rdf:type <%s> ; \
                   <%s> %d . \
                ?o om:hasValue ?o_measure . \
                ?o_measure om:hasNumericalValue ?%s . \
                }""" % (getShortName(variable_instance), historicalData_instance, ONTODOE_REFERSTO, instance_class, ONTODOE_POSITIONALID, int(positionalID), getShortName(variable_instance))
    else:
        query = PREFIX_OM + \
                PREFIX_RDF + \
                """SELECT DISTINCT ?rxnexp ?%s \
                WHERE { \
                <%s> <%s> ?rxnexp . \
                ?rxnexp ?p ?o . \
                ?o rdf:type <%s> . \
                ?o om:hasValue ?o_measure . \
                ?o_measure om:hasNumericalValue ?%s . \
                }""" % (getShortName(variable_instance), historicalData_instance, ONTODOE_REFERSTO, instance_class, getShortName(variable_instance))
    
    # Perform query
    response = performQuery(endpoint, query)
    return response

def getDoEStrategy(endpoint, strategy_instance):
    """
        This method retrieves information given instance of OntoDoE:Strategy.

        Arguments:
            endpoint - SPARQL Query endpoint
            strategy_instance - IRI of instance of OntoDoE:Strategy
    """
    # Delete "<" and ">" around the IRI
    strategy_instance = trimIRI(strategy_instance)

    # Construct query string
    query = PREFIX_RDF + \
            PREFIX_OWL + \
            """SELECT ?strategy \
            WHERE { \
            <%s> rdf:type ?strategy . \
            FILTER(?strategy != owl:Thing && ?strategy != owl:NamedIndividual) . \
            }""" % (strategy_instance)
    
    # Perform SPARQL query
    response = performQuery(endpoint, query)

    # Extract from the response
    if (len(response) > 1):
        raise Exception("Strategy Instance <%s> should only have one rdf:type." % (strategy_instance))
    res = [list(r.values())[0] for r in response]
    strategy_dict = {}
    strategy_dict[getShortName(res[0])] = getTSEMOSettings(endpoint, strategy_instance)
    return strategy_dict

def getTSEMOSettings(endpoint, tsemo_instance):
    """
        This method retrieves the settings of TSEMO algorithm as part of `Summit` package from the given instance of OntoDoE:TSEMO.
        (For TSEMO algorithm in package `Summit`, please visit: https://gosummit.readthedocs.io/en/latest/strategies.html#tsemo)

        Arguments:
            endpoint - SPARQL Query endpoint
            tsemo_instance - IRI of instance of OntoDoE:TSEMO
    """
    # Delete "<" and ">" around the IRI
    tsemo_instance = trimIRI(tsemo_instance)

    # Check if given tsemo_instance is an instance of OntoDoE:TSEMO
    if not checkInstanceClass(endpoint, tsemo_instance, ONTODOE_TSEMO):
        raise Exception("Instance <"+tsemo_instance+"> is not an instance of "+ONTODOE_TSEMO)
    
    # Construct query string
    query = """SELECT ?nGenerations ?nRetries ?nSpectralPoints ?populationSize \
            WHERE { \
            OPTIONAL {
                <%s> <%s> ?nGenerations . \
                <%s> <%s> ?nRetries . \
                <%s> <%s> ?nSpectralPoints . \
                <%s> <%s> ?populationSize . \
            } \
            }""" % (tsemo_instance, ONTODOE_NGENERATIONS, tsemo_instance, ONTODOE_NRETRIES, tsemo_instance, ONTODOE_NSPECTRALPOINTS, tsemo_instance, ONTODOE_POPULATIONSIZE)

    # Perform SPARQL query
    response = performQuery(endpoint, query)
    if (len(response) > 1):
        raise Exception("Instance <%s> should only have one set of settings." % (tsemo_instance))
    return response[0]

def checkInstanceClass(endpoint, instance, instance_class):
    """
        This method checks if the given instance is instantiated from the given instance class.

        Arguments:
            endpoint - SPARQL Query endpoint
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
    response = performQuery(endpoint, query)
    res = [list(r.values())[0] for r in response]
    if res[0] == 'true':
        return True
    else:
        return False

def checkIfPositionalIDExists(endpoint, instance):
    """
        This method checks if a OntoDoE:positionalID exists for a given instance.

        Arguments:
            endpoint - SPARQL Query endpoint
            instance - IRI of an instance of interest
    """
    # Delete "<" and ">" around the IRI
    instance = trimIRI(instance)

    # Prepare query string
    query = """SELECT ?result \
            WHERE { \
                BIND (IF ( EXISTS {<%s> <%s> ?id}, "true", "false" ) AS ?result) \
            }""" % (instance, ONTODOE_POSITIONALID)
    
    # Perform query
    response = performQuery(endpoint, query)
    res = [list(r.values())[0] for r in response]
    if res[0] == 'true':
        return True
    else:
        return False

def performQuery(endpoint, query):
    """
        This function performs query to knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint
            query - SPARQL Query string
    """
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    response = KGClient.execute(query)
    return json.loads(response)

def performUpdate(endpoint, update):
    """
        This function performs SPARQL Update to knowledge graph.

        Arguments:
            endpoint - SPARQL Update endpoint
            update - SPARQL Update string
    """
    KGClient = jpsBaseLib_view.RemoteStoreClient()
    KGClient.setUpdateEndpoint(endpoint)
    KGClient.executeUpdate(update)

def uploadOntology(triple_store_server, triple_store_repo, filePath):
    """
        This function uploads ontology to knowledge graph.

        Arguments:
            triple_store_server - address of triple store server, e.g. "http://kg.cmclinnovations.com:81/blazegraph"
            triple_store_repo - address of triple store repository, e.g. "testontorxn"
            filePath - the file path of ontology to be uploaded
    """
    KRClient = jpsBaseLib_view.KnowledgeRepository()
    KRClient.uploadOntology(triple_store_server, triple_store_repo, filePath)

def getShortName(iri):
    """
        This function gets the final part after the last '#' or '/' of an IRI.
        For example, it will return 'RxnExp_1' given 'https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1#RxnExp_1' or 'https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1'.
        
        Arguments:
            iri - IRI of interest
    """
    iri = trimIRI(iri)
    # Raise exception if the provided IRI ends with '#' or '/'
    if iri.endswith('#') or iri.endswith('/'):
        raise Exception(f"The IRI <{iri}> is not provided in correct format. It should NOT end with '#' or '/' when retrieving its shortname.")
    if '#' in iri:
        temp_iri = iri[iri.rfind('#')+1:]
        # Check if the parts after '#' is one string without separation, i.e. it should be 'shortname', instead of 'short/name'
        if not '/' in temp_iri:
            return temp_iri
        else:
            # Make sure return only the final part of the shortname
            return temp_iri[temp_iri.rfind('/')+1:]
    else:
        return iri[iri.rfind('/')+1:]

def getNameSpace(iri):
    """
        This method gets the namespace of a given IRI.
        For example, it will return 'https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1#' given 'https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1#RxnExp_1'.
        If given 'https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1', it will return 'https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/'.
        
        Arguments:
            iri - IRI of interest
    """
    iri = trimIRI(iri)
    if '#' in iri:
        return iri[:iri.rfind('#')+1]
    else:
        return iri[:iri.rfind('/')+1]

def trimIRI(iri):
    """
        This method deletes the '<' and '>' around the given IRI.

        Arguments:
            iri - IRI of interest
    """
    if isinstance(iri, list):
        for i in range(len(iri)):
            iri[i] = trimIRI(iri[i])
    else:
        if iri.startswith("<"):
            iri = iri[1:]
        if iri.endswith(">"):
            iri = iri[:-1]
    return iri
