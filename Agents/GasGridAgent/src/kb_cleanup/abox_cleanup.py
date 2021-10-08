"""
    Script to clean up initial time series data implementation in KG

    Author: mh807<@>cam.ac.uk
"""

import os
import time
import json

# get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module
from gasgridagent.jpsSingletons import jpsBaseLibView
# get settings and functions from kg_utils module
import gasgridagent.kg_utils as kg


def count_all_instances(query_endpoint):
    """
        Counts total number of triples in KG

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
    """

    var = 'triples'

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint)

    # Perform SPARQL query
    query = '''SELECT (COUNT(*) as ?%s) \
               WHERE { ?s ?p ?o }''' % var

    response = KGClient.execute(query)
    response = json.loads(response)

    return int(response[0][var])


def count_all_gas_amount_instances(query_endpoint):
    """
        Counts total number of instantiated gas amounts in KG

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
    """

    var = 'triples'

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint)

    # Perform SPARQL query
    query = kg.create_sparql_prefix('comp') + \
            kg.create_sparql_prefix('compa') + \
            kg.create_sparql_prefix('rdf') + \
            kg.create_sparql_prefix('xsd') + \
            '''SELECT (COUNT(distinct ?s) as ?%s) 
               WHERE { ?s rdf:type comp:IntakenGas; \
                          ^comp:hasTaken ?b }''' % var

    response = KGClient.execute(query)
    response = json.loads(response)

    return int(response[0][var])


def count_word_in_rdf_file(file, word):
    """
        Counts total number of occurrences of 'word' in rdf file 'file'

        Arguments:
            file - absolute path to rdf file.
            word - String of word to search.
    """

    with open(file, 'r') as f:
        data = f.read()
        total = data.count(word)

    return total


def delete_old_timeseries_triples(update_endpoint, offset, limit):
    """
        Deletes all triples associated with initial time series representation of gas flow data

        Arguments:
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            offset - offset for SPARQL query
            limit - limit for SPARQL query
    """

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(update_endpoint, update_endpoint)
    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = kg.create_sparql_prefix('comp') + \
            kg.create_sparql_prefix('compa') + \
            kg.create_sparql_prefix('rdf') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('xsd') + \
            '''DELETE {?a comp:hasTaken ?b. \
                       ?b rdf:type comp:IntakenGas; \
                          comp:atUTC ?c. \
                       ?d rdf:type om:VolumetricFlowRate; \
                          om:hasPhenomenon ?b;
                          om:hasValue ?e. \
                       ?e rdf:type om:Measure; \
                          om:hasNumericalValue ?f; \
                          om:hasUnit om:cubicMetrePerSecond-Time. }
               WHERE { SELECT ?a ?b ?c ?d ?e ?f
                       WHERE { ?a rdf:type comp:GasTerminal; \
                                  comp:hasTaken ?b. \
                               ?b rdf:type comp:IntakenGas; \
                                  comp:atUTC ?c. \
                               ?d rdf:type om:VolumetricFlowRate; \
                                  om:hasPhenomenon ?b;
                                  om:hasValue ?e. \
                               ?e rdf:type om:Measure; \
                                  om:hasNumericalValue ?f; \
                                  om:hasUnit om:cubicMetrePerSecond-Time. } \
                       ORDER BY ?f \
                       OFFSET %i LIMIT %i }''' % (offset, limit)

    KGClient.executeUpdate(query)


def delete_unattached_timeseries_triples(update_endpoint, offset, limit):
    """
        Deletes all gas flow measurement triples which are not connected to a comp:GasTerminal instance

        Arguments:
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            offset - offset for SPARQL query
            limit - limit for SPARQL query
    """

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(update_endpoint, update_endpoint)
    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = kg.create_sparql_prefix('comp') + \
            kg.create_sparql_prefix('compa') + \
            kg.create_sparql_prefix('rdf') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('ts') + \
            '''DELETE {?a comp:hasTaken ?b; \
                          ?c ?d . \
                       ?b rdf:type comp:IntakenGas; \
                          comp:atUTC ?e . \
                       ?f rdf:type om:VolumetricFlowRate; \
                          om:hasPhenomenon ?b; \
                          om:hasValue ?g . \
                       ?g rdf:type om:Measure; \
                          om:hasUnit om:cubicMetrePerSecond-Time; \
                          om:hasNumericalValue ?h; \
                          ts:hasTimeSeries ?i . \
                       ?i ?j ?k } \
               WHERE { SELECT ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k \
                       WHERE { ?a comp:hasTaken ?b . \
                               OPTIONAL { ?a ?c ?d } \
                               ?b rdf:type comp:IntakenGas. \
                               OPTIONAL { ?b comp:atUTC ?e } \
                               ?f rdf:type om:VolumetricFlowRate; \
                                  om:hasPhenomenon ?b; \
                                  om:hasValue ?g . \
                               ?g rdf:type om:Measure; \
                                  om:hasUnit om:cubicMetrePerSecond-Time. \
                               OPTIONAL { ?g om:hasNumericalValue ?h } \
                               OPTIONAL { ?g ts:hasTimeSeries ?i . \
                                          ?i ?j ?k } \
                       FILTER NOT EXISTS {?a rdf:type comp:GasTerminal } \
                               } \
                       ORDER BY ?g \
                       OFFSET %i LIMIT %i }''' % (offset, limit)

    KGClient.executeUpdate(query)


def clean_up_old_attached_timeseries(chunk):
    """
        Clean up existing KG wrt old gas flow time series triples attached to any Gas Terminal instance

        Arguments:
            chunk - chunk size of individual cleanup runs, i.e. limit for respective SPARQL queries.
    """

    print('\n##### Cleanup of old gas flow triples attached to gas terminals #####\n')

    # Read properties file incl. SPARQL endpoints
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Initial KG summary statistics
    triples_old = count_all_instances(kg.QUERY_ENDPOINT)
    gas_amounts_old = count_all_gas_amount_instances(kg.QUERY_ENDPOINT)
    print('Before clean up:')
    print('Total number of triples: {}'.format(triples_old))
    print('Total number of old gas amounts: {} \n'.format(gas_amounts_old))

    s = int(time.time())
    print('Time before cleanup: {}'.format(s))

    # Run consecutive updates of size 'chunk'
    offset = 0
    total_deleted = 0
    total_screened = 0
    further_triples = True

    while further_triples:

        print('Counting batch offset: {0: >8} limit: {1: >8}'.format(offset, chunk))

        # Total number of remaining triples
        triples = count_all_instances(kg.QUERY_ENDPOINT)

        # Clean up (portion of) KG
        delete_old_timeseries_triples(kg.UPDATE_ENDPOINT, offset, chunk)

        # If triples have been deleted in currently screened portion of KG
        if triples > count_all_instances(kg.QUERY_ENDPOINT):
            new_deleted = (triples - count_all_instances(kg.QUERY_ENDPOINT))
            total_deleted += new_deleted
            total_screened += new_deleted
            deleted = True
        else:
            deleted = False

        # Update offset only if no data has been deleted in that chunk
        if not deleted:
            offset += chunk
            total_screened += chunk

        # End successive partial SPARQL update if all triples have been screened
        if total_screened > triples_old:
            further_triples = False

    e = int(time.time())
    print('Time after cleanup: {}'.format(e))
    print('Cleanup duration in s: {}\n'.format(e - s))

    # Final summary statistics
    triples_new = count_all_instances(kg.QUERY_ENDPOINT)
    gas_amounts_new = count_all_gas_amount_instances(kg.QUERY_ENDPOINT)
    print('After clean up:')
    print('Total number of triples: {}'.format(triples_new))
    print('Total number of old gas amounts: {} \n'.format(gas_amounts_new))

    # Each initial gas flow measurement was represented by n triples
    n = 9
    if triples_old == triples_new:
        print('No triples have been deleted.\n')
    else:
        if (triples_old - triples_new) == (9 * (gas_amounts_old - gas_amounts_new)):
            print('SUCCESS: Only outdated gas flow measurements have been deleted.\n')
        else:
            print('WARNING: Triples beyond outdated gas flow measurements have been deleted.')
            print('Number of additionally deleted triples: {} \n'.format(triples_old - triples_new))


def clean_up_old_unattached_timeseries(chunk):
    """
        Clean up existing KG wrt old gas flow time series triples not attached to any Gas Terminal instance

        Arguments:
            chunk - chunk size of individual cleanup runs, i.e. limit for respective SPARQL queries.
    """

    print('\n##### Cleanup of old gas flow triples NOT attached to gas terminals #####\n')

    # Read properties file incl. SPARQL endpoints
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Initial KG summary statistics
    triples_old = count_all_instances(kg.QUERY_ENDPOINT)
    gas_amounts_old = count_all_gas_amount_instances(kg.QUERY_ENDPOINT)
    print('Before clean up:')
    print('Total number of triples: {}'.format(triples_old))
    print('Total number of old gas amounts: {} \n'.format(gas_amounts_old))

    s = int(time.time())
    print('Time before cleanup: {}'.format(s))

    # Run consecutive updates of size 'chunk'
    offset = 0
    total_deleted = 0
    total_screened = 0
    further_triples = True

    while further_triples:

        print('Counting batch offset: {0: >8} limit: {1: >8}'.format(offset, chunk))

        # Total number of remaining triples
        triples = count_all_instances(kg.QUERY_ENDPOINT)

        # Clean up (portion of) KG
        delete_unattached_timeseries_triples(kg.UPDATE_ENDPOINT, offset, chunk)

        # If triples have been deleted in currently screened portion of KG
        if triples > count_all_instances(kg.QUERY_ENDPOINT):
            new_deleted = (triples - count_all_instances(kg.QUERY_ENDPOINT))
            total_deleted += new_deleted
            total_screened += new_deleted
            deleted = True
        else:
            deleted = False

        # Update offset only if no data has been deleted in that chunk
        if not deleted:
            offset += chunk
            total_screened += chunk

        # End successive partial SPARQL update if all triples have been screened
        if total_screened > triples_old:
            further_triples = False

    e = int(time.time())
    print('Time after cleanup: {}'.format(e))
    print('Cleanup duration in s: {}\n'.format(e - s))

    # Final summary statistics
    triples_new = count_all_instances(kg.QUERY_ENDPOINT)
    gas_amounts_new = count_all_gas_amount_instances(kg.QUERY_ENDPOINT)
    print('After clean up:')
    print('Total number of triples: {}'.format(triples_new))
    print('Total number of old gas amounts: {} \n'.format(gas_amounts_new))


def delete_incomplete_new_timeseries_triples(update_endpoint):
    """
        Deletes all triples associated with incomplete (unintentional) new time series instantiation
        !! All triples deleted by this function are connected to a comp:GasTerminal instance via :hasTaken!!

        Arguments:
            update_endpoint - SPARQL Update endpoint for knowledge graph.
    """

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(update_endpoint, update_endpoint)
    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = kg.create_sparql_prefix('comp') + \
            kg.create_sparql_prefix('compa') + \
            kg.create_sparql_prefix('rdf') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('ts') + \
            '''DELETE {?a comp:hasTaken ?b. \
                       ?b rdf:type comp:IntakenGas. \
                       ?c rdf:type om:VolumetricFlowRate; \
                          om:hasPhenomenon ?b; \
                          om:hasValue ?d. \
                       ?d rdf:type om:Measure; \
                          om:hasUnit om:cubicMetrePerSecond-Time; \
                          ts:hasTimeSeries ?e. \
                       ?e ?f ?g } \
               WHERE { SELECT ?a ?b ?c ?d ?e ?f ?g \
                       WHERE { ?a rdf:type comp:GasTerminal; \
                                  comp:hasTaken ?b. \
                               ?b rdf:type comp:IntakenGas. \
                               ?c rdf:type om:VolumetricFlowRate; \
                                  om:hasPhenomenon ?b;
                                  om:hasValue ?d. \
                               ?d rdf:type om:Measure; \
                                  om:hasUnit om:cubicMetrePerSecond-Time. \
                               OPTIONAL { ?d ts:hasTimeSeries ?e. \
                                          ?e ?f ?g } \
                               } \
               }'''

    KGClient.executeUpdate(query)


# USE WITH CAUTION!!
def delete_terminal(terminal_label):
    """
        Delete all triples associated with Gas Terminal with given rdfs_label

        Arguments:
            terminal_label - rdfs label for terminal to be deleted.
    """

    # Read properties file incl. SPARQL endpoints
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(kg.UPDATE_ENDPOINT, kg.UPDATE_ENDPOINT)

    # Run delete query
    query = kg.create_sparql_prefix('rdfs') + \
            kg.create_sparql_prefix('xsd') + \
            '''DELETE { ?s ?p ?o } \
               WHERE { ?s rdfs:label "%s"^^xsd:string; \
                          ?p ?o }''' % (terminal_label)
    KGClient.executeUpdate(query)


def correct_terminal_spelling():
    """
        Correct spelling of "Theddlethorpe Terminal"
    """

    # Define incorrect and corrected objects
    wrong_IRI = 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/TheddlethorpeTermin'
    wrong_name = 'Theddlethorpe Termin'
    correct_IRI = 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/TheddlethorpeTerminal'
    correct_name = 'Theddlethorpe Terminal'

    # Read properties file incl. SPARQL endpoints
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(kg.UPDATE_ENDPOINT, kg.UPDATE_ENDPOINT)

    # 1) Correct wrong spelling of terminal IRI
    query = kg.create_sparql_prefix('rdfs') + \
            kg.create_sparql_prefix('xsd') + \
            '''DELETE { <%s> ?p ?o } \
               INSERT { <%s> ?p ?o } \
               WHERE { <%s> ?p ?o }''' % (wrong_IRI, correct_IRI, wrong_IRI)
    KGClient.executeUpdate(query)

    # 2) Correct wrong spelling of terminal label
    query = kg.create_sparql_prefix('rdfs') + \
            kg.create_sparql_prefix('xsd') + \
            '''DELETE DATA { <%s> rdfs:label "%s"^^xsd:string }''' % (correct_IRI, wrong_name)
    KGClient.executeUpdate(query)
    query = kg.create_sparql_prefix('rdfs') + \
            kg.create_sparql_prefix('xsd') + \
            '''INSERT DATA { <%s> rdfs:label "%s"^^xsd:string }''' % (correct_IRI, correct_name)
    KGClient.executeUpdate(query)


# Entry point, calls main function
if __name__ == '__main__':

    # #1) Derive (approximate) number of old gas flow measurements (only to double check numbers from SPARQL queries
    # #   if local copy of rdf file is present)
    # #Path to rdf file
    # f = os.path.abspath(r'C:\Users\mh807.CEB-315-55-WLO\Documents\99 Projects\3 UK GasGrid'
    #                     r'\0 Ontologies and Instantiation\Time series data\ontogasgrid.rdf')
    # # Word/relationship to look for
    # w = 'hasTaken'
    # print('Total number of \'{}\' in rdf file: {}'.format(w, count_word_in_rdf_file(f, w)))

    # Define (max) number of results per SPARQL query (for 'limit' and 'offset' of SPARQL queries)
    chunk = 50

    # Perform 2 cleanup runs to account for different namespaces used for 'comp' and 'compa'
    for run in range(2):
        if run == 1:
            kg.PREFIXES['comp'] = 'http://www.theworldavatar.com/ontology/ts_backup/gas_network_components.owl#'
            kg.PREFIXES['compa'] = 'http://www.theworldavatar.com/kb/ts_backup/offtakes_abox/'

        print('\nCurrent namespaces:')
        print('"comp"  : {}\n"compa" : {}\n'.format(kg.PREFIXES['comp'], kg.PREFIXES['compa']))

        # 2) Delete old gas flow measurements
        #clean_up_old_attached_timeseries(chunk)

        # 3) Delete (old) unattached gas flow measurements
        #clean_up_old_unattached_timeseries(chunk)

    # Reset default namespaces
    kg.PREFIXES['comp'] = 'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#'
    kg.PREFIXES['compa'] = 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/'

    # 4) Delete existing incomplete gas flow measurement time series instantiations
    #delete_incomplete_new_timeseries_triples(kg.UPDATE_ENDPOINT)

    # 5) Delete duplicate of "Theddlethorpe Terminal"
    #terminal_label = 'Theddlethorpe Terminal'
    #delete_terminal(terminal_label)

    # 6) Correct wrong spelling of "Theddlethorpe Terminal"
    #correct_terminal_spelling()

    print('Total number of remaining instances: {}'.format(count_all_instances(kg.QUERY_ENDPOINT)))
