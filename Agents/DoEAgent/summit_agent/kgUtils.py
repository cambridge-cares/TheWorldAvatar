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


    # doe = { \
    #             "TSEMO": {"nSpectralPoints": 30, "nGenerations": 20, "populationSize": 20}, \
    #             "continuousVariables": [{"name": "ContinuousVariable_1", "lower": 1, "upper": 10}, 
    #             {"name": "ContinuousVariable_2", "lower": 0.02, "upper": 0.2},
    #             {"name": "ContinuousVariable_3", "lower": 5, "upper": 15},
    #             {"name": "ContinuousVariable_4", "lower": 30, "upper": 70}], \
    #             "systemResponses": [{"name": "SystemResponse_1", "direction": "maximise"}, 
    #             {"name": "SystemResponse_2", "direction": "minimise"}], \
    #             "historicalData": previous_results, \
    #             "numOfExp": 1}

def constructHistoricalDataTable(endpoint, domain_instance, systemResponse_instances, historicalData_instance):
    """
    systemResponse_instance is expected to be a list of system responses, i.e. even there's only one system response we are interested in, 
    it should be provided in the form of a list, e.g. ['https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1']
    """
    domain_instance = trimIRI(domain_instance)
    historicalData_instance = trimIRI(historicalData_instance)
    
    list_of_designVariable_dict = []
    list_of_systemResponse_dict = []

    list_of_hist_data_df = []

    list_of_design_variables = getDesignVariables(endpoint, domain_instance)
    for var in list_of_design_variables:
        # Prepare data for dictionary of design variable
        des_var = {}
        des_var['name'] = getShortName(var['var'])
        des_var['description'] = getShortName(var['clz']) + "_" + ( str(var['id'] if 'id' in var else None) )
        des_var['lower'] = var['lower'] if 'lower' in var else None
        des_var['upper'] = var['upper'] if 'upper' in var else None
        list_of_designVariable_dict.append(des_var)
        # Prepare data for the historical data table
        data = getHistoricalDataOfVariable(endpoint, historicalData_instance, var['var'], var['clz'], var['id'] if 'id' in var else None)
        _to_df = {}
        for k in data[0]:
            _to_df[k] = tuple(d[k] for d in data)
        list_of_hist_data_df.append(pd.DataFrame.from_dict(_to_df))
    # name: getShortName(var), description: getShortName(clz)+_id, lower: , upper: 
    # name: getShortName(res), description: getShortName(clz)+_id direction: direction
    list_of_system_response = getSystemResponses(endpoint, systemResponse_instances)
    for res in list_of_system_response:
        # Prepare data for dictionary of system response
        sys_res = {}
        sys_res['name'] = getShortName(res['res'])
        sys_res['description'] = getShortName(res['clz']) + "_" + ( str(res['id'] if 'id' in res else None) )
        sys_res['direction'] = 'maximise' if 'true' in res['maximise'] else "minimise"
        list_of_systemResponse_dict.append(sys_res)
        # Prepare data for the historical data table
        data = getHistoricalDataOfVariable(endpoint, historicalData_instance, res['res'], res['clz'], res['id'] if 'id' in res else None)
        _to_df = {}
        for k in data[0]:
            _to_df[k] = tuple(d[k] for d in data)
        list_of_hist_data_df.append(pd.DataFrame.from_dict(_to_df))
    
    historicalData_df = reduce(lambda df1, df2: pd.merge(df1, df2, on='rxnexp'), list_of_hist_data_df)
    return {"continuousVariables": list_of_designVariable_dict}, {"systemResponses": list_of_systemResponse_dict}, historicalData_df

def getDesignVariables(endpoint, domain_instance):
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

def getSystemResponses(endpoint, systemRespons_instances):
    list_sys = []
    for sys_ins in systemRespons_instances:
        sys_ins = trimIRI(sys_ins)
        # Prepare query string
        query = """SELECT DISTINCT ?clz ?id ?maximise \
                WHERE { \
                <%s> <%s> ?clz . 
                OPTIONAL {<%s> <%s> ?id} \
                OPTIONAL {<%s> <%s> ?maximise} \
                }""" % (sys_ins, ONTODOE_REFERSTO, sys_ins, ONTODOE_POSITIONALID, sys_ins, ONTODOE_MAXIMISE)
        response = performQuery(endpoint, query)
        response[0]['res'] = sys_ins
        list_sys += response
    return list_sys

def getHistoricalDataOfVariable(endpoint, historicalData_instance, variable_instance, instance_class, positionalID=None):
    """This method should work for both continuous variable as part of domain, and system response
    """
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
    """
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
    """

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

# This function performs query to knowledge graph
def performQuery(endpoint, query):
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    response = KGClient.execute(query)
    return json.loads(response)

def getShortName(iri):
    iri = trimIRI(iri)
    if '#' in iri:
        return iri[iri.rfind('#')+1:]
    else:
        return iri[iri.rfind('/')+1:]

def trimIRI(iri):
    if iri.startswith("<"):
        iri = iri[1:]
    if iri.endswith(">"):
        iri = iri[:-1]
    return iri
