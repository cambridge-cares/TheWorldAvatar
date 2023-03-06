################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# flood warnings and alerts from the API and instantiate it in the KG

import uuid
from datetime import datetime as dt

from agent.datamodel.data_mapping import TIMECLASS, DOUBLE
from agent.datainstantiation.ea_data import retrieve_current_warnings, \
                                            retrieve_flood_area_data
from agent.datainstantiation.ons_data import retrieve_ons_county
from agent.kgutils.tsclient import TSClient
from agent.kgutils.querytemplates import *
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from agent.utils.stackclients import GdalClient, GeoserverClient, \
                                     OntopClient, PostGISClient, \
                                     create_geojson_for_postgis

# Import PyDerivationAgent for derivation markup
from pyderivationagent import PyDerivationClient
from pyderivationagent import PySparqlClient
from agent.datainstantiation.derivation_markup import retrieve_affected_property_info, \
                                                      flood_assessment_derivation_markup, \
                                                      retrieve_flood_assessment_derivation_iri

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def update_warnings(county=None, mock_api=None, query_endpoint=QUERY_ENDPOINT, 
                    update_endpoint=UPDATE_ENDPOINT):
    """
    Update instantiated flood warnings/alerts incl. associated flood areas in the KG, 
    i.e. instantiate missing ones, update existing ones, and archive outdated ones
    NOTE: Archiving is not yet implemented as there are issues with creating String 
          TimeSeries via py4jps; hence, outdated triples are simply deleted

    Arguments:
        county (str): County name for which to instantiate flood warnings (e.g. 'Hampshire')
                      Instantiates ALL current warnings if no county given
        mock_api (str): Path to local .json file to mock API response
    """

    # Initialise return values
    instantiated_areas = 0
    instantiated_warnings = 0
    updated_warnings = 0
    deleted_warnings = 0

    # Initialise KG Client with PySparqlClient instance
    kg_client = PySparqlClient(query_endpoint=query_endpoint,
                               update_endpoint=update_endpoint)

    # Create a PyDerivationClient instance
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=query_endpoint,
        update_endpoint=update_endpoint,
    )

    if county:
        ### Update flood warnings for given county ###
        #TODO: to be implemented, see note in agent\flaskapp\inputtasks\routes.py
        logger.warning("County-specific instantiation not yet implemented")
    else:
        ### Update all flood warnings ###
        # 1) Retrieve current flood warnings from API
        logger.info("Retrieving current flood warnings from API ...")
        current_warnings = retrieve_current_warnings(mock_api=mock_api)
        logger.info("Current flood warnings retrieved.")

        # 2) Retrieve instantiated flood warnings and flood areas from KG
        logger.info("Retrieving instantiated flood warnings and areas from KG ...")
        areas_kg = get_instantiated_flood_areas(kgclient=kg_client)  
        warnings_kg = get_instantiated_flood_warnings(kgclient=kg_client)      
        logger.info("Instantiated warnings and areas retrieved.")

        # 3) Extract flood warnings and flood areas ...
        area_uris = [w.get('area_uri') for w in current_warnings]
        area_uris = [a for a in area_uris if a is not None]
        warning_uris = [w.get('warning_uri') for w in current_warnings]
        warning_uris = [w for w in warning_uris if w is not None]       
        last_altered = {w['warning_uri']: w['last_altered'] for w in current_warnings}
        # ... to be instantiated (i.e. not yet in KG)
        areas_to_instantiate = [a for a in area_uris if a not in areas_kg]
        warnings_to_instantiate = [w for w in warning_uris if w not in warnings_kg]
        # ... to be updated (i.e. outdated information in KG)
        warnings_to_update = [w for w in warning_uris if w in warnings_kg]
        warnings_to_update = [w for w in warnings_to_update if warnings_kg.get(w) < last_altered.get(w)]
        # ... to be deleted (i.e. not active anymore)
        warnings_to_delete = [w for w in warnings_kg if w not in warning_uris]

        # 4) Instantiate missing flood areas and warnings
        if warnings_to_instantiate or areas_to_instantiate:
            print('Instantiating flood warnings and areas ...')
            instantiated_areas, instantiated_warnings = \
                instantiate_flood_areas_and_warnings(areas_to_instantiate, areas_kg,
                                                     warnings_to_instantiate, current_warnings, 
                                                     kgclient=kg_client, 
                                                     derivation_client=derivation_client)
            print('Instantiation finished.')

        # 5) Update outdated flood warnings
        if warnings_to_update:
            print('Updating flood warnings ...')
            updated_warnings = \
                update_instantiated_flood_warnings(warnings_to_update, current_warnings, 
                                                   kgclient=kg_client)
            print('Updating finished.')

        # 6) Delete inactive flood warnings
        if warnings_to_delete:
            print('Deleting inactive flood warnings ...')
            deleted_warnings, drop_times_iris = \
                delete_instantiated_flood_warnings(warnings_to_delete, kgclient=kg_client)
            print('Deleting finished.')

            # Derivation markup:
            # Drop timestamps of deleted flood warnings & derivation instances
            derivation_client.dropTimestampsOf(drop_times_iris)

        # Print update summary 
        print(f'Instantiated areas: {instantiated_areas}')
        print(f'Instantiated warnings: {instantiated_warnings}')
        print(f'Updated warnings: {updated_warnings}')
        print(f'Deleted warnings: {deleted_warnings}')

    return instantiated_areas, instantiated_warnings, updated_warnings, deleted_warnings


def instantiate_flood_areas_and_warnings(areas_to_instantiate: list, areas_kg: dict,
                                         warnings_to_instantiate: list, warnings_data_api: list,
                                         query_endpoint=QUERY_ENDPOINT, kgclient=None,
                                         derivation_client=None):
    """
    Instantiate flood areas and warnings in the KG with data dicts as retrieved 
    from API by 'retrieve_current_warnings' (flood area data retrieved as needed)

    Arguments:
        areas_to_instantiate (list): List of flood area URIs to be instantiated
        areas_kg (dict): Dictionary with instantiated flood area URIs as keys and flood area IRIs as values
        warnings_to_instantiate (list): List of flood warning URIs to be instantiated
        warnings_data_api (list): List of dictionaries with flood warning data from API
        derivation_client (PyDerivationClient): PyDerivationClient instance

    Returns:
        Number of newly instantiated flood areas and flood warnings as int
    """

    # Initialise return values
    new_areas = 0
    new_warnings = 0

    # Create PySparqlClient instance if not provided
    if not kgclient:
        kgclient = PySparqlClient(query_endpoint=query_endpoint,
                                  update_endpoint=query_endpoint)

    # Instantiate (potentially) missing flood areas
    if areas_to_instantiate:
        # Initialise TimeSeries Client
        #TODO: Uncomment once a solution for Java String class retrieval has been found
        #ts_client = TSClient(kg_client=kgclient, timeclass=TIMECLASS)

        # Retrieve data for flood areas from API
        areas_data_to_instantiate = []
        logger.info('Retrieving missing flood areas from API ...')
        for area in areas_to_instantiate:
            areas_data_to_instantiate.append(retrieve_flood_area_data(area))
        logger.info('Missing flood areas retrieved.')

        # Instantiate Blazegraph triples
        new_areas, area_location_map, area_history_map = \
            instantiate_flood_areas(areas_data_to_instantiate, kgclient=kgclient)
        
        # Initialise time series to store history of key characteristics of flood 
        # area's warnings and alerts, i.e. timestamp of change, severity, message, 
        # type of flood, attached water body, total number of affected people & 
        # buildings, total value of affected buildings
        #NOTE: Idea is to store all of these information as ONE string delimited by
        #      a special character, e.g. '|' --> available for later analysis without
        #      too much time series overhead
        #      --> currently facing issues to initialise time series with String column
        #          type via py4jps
        #TODO: Finalise implementation once a solution for declaring Java String class
        #       in Python via py4jps has been found
        # Initialise FloodArea's warning and alert history time series
        # with ts_client.connect() as conn:
        #     ### List of lists
        #     dataIRIs = [[datairi] for datairi in area_history_map.values()]
        #     # Example for declaring Java class --> equivalent for String required
        #     data_class = DOUBLE
        #     dataClasses = [[data_class] for i in dataIRIs]
        #     ### Lists
        #     timeFormats = [TIME_FORMAT] * len(dataIRIs)
        #     # Initialise time series
        #     ts_client.tsclient.bulkInitTimeSeries(dataIRIs, dataClasses, timeFormats, conn)
    else:
        area_location_map = {}

    # Create overarching mapping between flood area URIs and location IRIs, i.e.
    # already instantiated and newly instantiated ones
    area_location_map.update(areas_kg)
    
    # Instantiate missing flood warnings
    warning_data_to_instantiate = [w for w in warnings_data_api if w.get('warning_uri') in warnings_to_instantiate]
    # Add location IRIs to warning data
    for w in warning_data_to_instantiate:
        w['location_iri'] = area_location_map.get(w.get('area_uri'))
    new_warnings = instantiate_flood_warnings(warning_data_to_instantiate, kgclient=kgclient,
                                              derivation_client=derivation_client)

    return new_areas, new_warnings


def instantiate_flood_areas(areas_data: list=[],
                            query_endpoint=QUERY_ENDPOINT,
                            kgclient=None):
    """
    Instantiate list of flood area data dicts as retrieved from API by 'retrieve_flood_area_data'

    Arguments:
        areas_data (list): List of dicts with relevant flood area data
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints

    Returns:
        Number (int) of instantiated flood areas
        Dict with newly instantiated area IRIs as keys and Location IRIs as values
        Dict with newly instantiated area IRIs as keys and History IRIs as values
    """

    # Create PySparqlClient instance if not provided
    if not kgclient:
        kgclient = PySparqlClient(query_endpoint=query_endpoint,
                                  update_endpoint=query_endpoint)

    # Initialise relevant Stack Clients and parameters
    postgis_client = PostGISClient()
    gdal_client = GdalClient()
    geoserver_client = GeoserverClient()

    triples = ''
    area_location_map = {}
    area_history_map = {}
    for area in areas_data:
        # Create IRIs for Location associated with flood area (and potential flood event)
        area['location_iri'] = KB + 'Location_' + str(uuid.uuid4())
        area['admin_district_iri'] = KB + 'AdministrativeDistrict_' + str(uuid.uuid4())
        area['areal_extend_iri'] = KB + 'ArealExtentPolygon_' + str(uuid.uuid4())
        # Create waterbody IRI
        area['waterbody_iri'] = KB + area['water_body_type'].capitalize() + '_' + str(uuid.uuid4())
        # Create history iri
        area['history_iri'] = KB + 'FloodAlertOrWarningHistory_' + str(uuid.uuid4())
        
        # Create dicts for newly created IRIs per flood area
        area_location_map[area['area_uri']] = area['location_iri']
        area_history_map[area['area_uri']] = area['history_iri']

        # Retrieve county IRI from ONS API
        logger.info("Retrieving county IRI from ONS API ...")
        area['county_iri'] = retrieve_ons_county(area['county'])

        # Create GeoJSON string for PostGIS upload
        logger.info("Create FloodArea GeoJSON string for PostGIS upload ...")
        props = ['areal_extend_iri', 'area_uri', 'polygon_uri', 'label', 
                 'area_types', 'water_body_type']
        props = {p: area[p] for p in props}
        geojson_str = create_geojson_for_postgis(**props, kg_endpoint=query_endpoint)

        # Remove dictionary keys not required for instantiation
        area.pop('county', None)
        poly_uri = area.pop('polygon_uri', None) 

        # 1) Prepare instantiation in KG (done later in bulk)
        triples += flood_area_instantiation_triples(**area)
        
        # 2) Upload polygon to PostGIS
        # Upload OBDA mapping and create Geoserver layer when first geospatial
        # data is uploaded to PostGIS
        if not postgis_client.check_table_exists():
            logger.info('Uploading OBDA mapping ...')
            OntopClient.upload_ontop_mapping()
            # Initial data upload required to create postGIS table and Geoserver layer            
            logger.info('Uploading GeoJSON to PostGIS ...')
            gdal_client.uploadGeoJSON(geojson_str)
            logger.info('Creating layer in Geoserver ...')
            geoserver_client.create_workspace()
            geoserver_client.create_postgis_layer()
        else:        
            # Upload new geospatial information
            if not postgis_client.check_flood_area_exists(area['area_uri'], poly_uri):
                logger.info('Uploading GeoJSON to PostGIS ...')
                gdal_client.uploadGeoJSON(geojson_str)

    # Create INSERT query and perform update
    query = f"INSERT DATA {{ {triples} }}"
    kgclient.performUpdate(query)

    return len(areas_data), area_location_map, area_history_map


def instantiate_flood_warnings(warnings_data: list=[], 
                               query_endpoint=QUERY_ENDPOINT,
                               kgclient=None, derivation_client=None):
    """
    Instantiate list of flood warning data dicts as retrieved from API by 'retrieve_current_warnings',
    further enriched with location_iri created by 'flood_area_instantiation_triples'

    Arguments:
        warnings_data (list): List of dicts with relevant flood warnings/alerts data
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints
        derivation_client (PyDerivationClient): PyDerivationClient instance

    Returns:
        Number (int) of instantiated flood warnings/alerts
    """

    # Initialise PostGIS client
    postgis_client = PostGISClient()

    # Create PySparqlClient instance if not provided
    if not kgclient:
        kgclient = PySparqlClient(query_endpoint=query_endpoint,
                                  update_endpoint=query_endpoint)

    triples = ''
    for warning in warnings_data:
        # Create IRI of potential flood event
        warning['flood_event_iri'] = KB + 'Flood_' + str(uuid.uuid4())

        # Remove dictionary keys not required for instantiation
        warning.pop('last_altered', None)
        
        # Construct triples for flood warning/alert instantiation
        triples += flood_warning_instantiation_triples(**warning)

        # Ensure flood area is set active (even if flood area itself has not been
        # newly instantiated, i.e. is already instantiated and likely inactive)
        num_rows = postgis_client.set_flood_area_activity(activity=True, area_uri=warning['area_uri'])
        if num_rows != 1:
            logger.error(f'Expected to change "active" field for 1 flood area, but updated {num_rows}.')
            raise RuntimeError(f'Expected to change "active" field for 1 flood area, but updated {num_rows}.')

        # Update current severity level for flood area (to allow better styling)
        num_rows = postgis_client.set_flood_area_severity(severity=SEVERITY_LEVELS[warning['severity'].lower()],
                                                          area_uri=warning['area_uri'])
        if num_rows != 1:
            logger.error(f'Expected to change "active" field for 1 flood area, but updated {num_rows}.')
            raise RuntimeError(f'Expected to change "active" field for 1 flood area, but updated {num_rows}.')
        
        # Derivation markup:
        if derivation_client:
            # 1) Retrieve list of affected buildings, i.e. buildings in flood area
            affected_building_iris = postgis_client.get_buildings_within_floodarea(warning['area_uri'])
            # 2) Retrieve building info for affected properties 
            property_info_dct = retrieve_affected_property_info(kgclient, affected_building_iris)
            property_value_iris = [property_info_dct[iri]['mv'] for iri in affected_building_iris if property_info_dct[iri]['mv']]
            print(f'Number of affected properties: {len(property_info_dct)}')
            # 3) Add derivation markup for the flood event
            logger.info(f"Adding derivation markup for warning: {warning['warning_uri']}")
            flood_assessment_derivation_markup(
                derivation_client=derivation_client,
                flood_warning_iri=warning['warning_uri'],
                affected_building_iris=affected_building_iris,
                property_value_iris=property_value_iris,
                flood_assessment_derivation_iri=retrieve_flood_assessment_derivation_iri(
                    sparql_client=kgclient,
                    flood_warning_iri=warning['warning_uri'],
                    flood_assessment_agent_iri=FLOOD_ASSESSMENT_AGENT_IRI,
                )
            )

    # Create INSERT query and perform update
    query = f"INSERT DATA {{ {triples} }}"
    kgclient.performUpdate(query)

    return len(warnings_data)


def update_instantiated_flood_warnings(warnings_to_update: list, warnings_data_api: list,
                                       query_endpoint=QUERY_ENDPOINT, kgclient=None,
                                       derivation_client=None):
    """
    Update flood warnings and alerts in the KG (i.e. update list of flood warnings
    with data dicts as retrieved from API by 'retrieve_current_warnings')

    Arguments:
        warnings_to_update (list): List of flood warning URIs to be updated
        warnings_data_api (list): List of dictionaries with flood warning data from API

    Returns:
        Number of updated flood warnings as int
    """

    # Initialise PostGIS client
    postgis_client = PostGISClient()

    # Create PySparqlClient instance if not provided
    if not kgclient:
        kgclient = PySparqlClient(query_endpoint=query_endpoint,
                                  update_endpoint=query_endpoint)

    # Extract relevant warning data
    data_to_update = [w for w in warnings_data_api if w.get('warning_uri') in warnings_to_update]

    for data in data_to_update:
        # Keep only "updatable" information
        relevant = ['warning_uri', 'severity', 'label', 'message', 'timeRaised', 
                    'timeMsgChanged', 'timeSevChanged'] 
        data = {k: v for k, v in data.items() if k in relevant}
        # Create SPARQL update query
        query = update_flood_warning(**data)
        # Perform update
        kgclient.performUpdate(query)

        # Retrieve associated flood area URI
        query = get_associated_flood_area(data['warning_uri'])
        res = kgclient.performQuery(query)
        # Unwrap results
        try:
            area = [r['area_iri'] for r in res][0]
        except Exception as ex:
            logger.error('No associated flood area IRI could be retrieved for warning: {}.'.format(data['warning_uri']))
            raise RuntimeError('No associated flood area IRI could be retrieved for warning: {}.'.format(data['warning_uri'])) from ex  

        # Update current severity level for flood area (to allow better styling)
        num_rows = postgis_client.set_flood_area_severity(severity=SEVERITY_LEVELS[data['severity'].lower()],
                                                          area_uri=area)
        if num_rows != 1:
            logger.error(f'Expected to change "severity" field for 1 flood area, but updated {num_rows}.')
            raise RuntimeError(f'Expected to change "severity" field for 1 flood area, but updated {num_rows}.')
        
        # Derivation markup:
        # 1) Update timestamp of flood warning as pure input
        derivation_client.updateTimestamps([data['warning_uri']])
        # 2) Request for derivation update for existing derivation IRI (i.e. 
        #    updates the timestamp of derivation instance +  request update)
        deriv_iri = retrieve_flood_assessment_derivation_iri(sparql_client=kgclient,
                        flood_warning_iri=data['warning_uri'],
                        flood_assessment_agent_iri=FLOOD_ASSESSMENT_AGENT_IRI)
        if derivation_client and deriv_iri:
            logger.info(f"Request derivation update for: {data['warning_uri']}")
            flood_assessment_derivation_markup(
                derivation_client=derivation_client,
                flood_assessment_derivation_iri=deriv_iri
            )
    
    return len(data_to_update)


def delete_instantiated_flood_warnings(warnings_to_delete: list, query_endpoint=QUERY_ENDPOINT, 
                                       kgclient=None):
    """
    Delete obsolete (i.e. inactive) flood warnings and alerts from the KG 
    (i.e. including all obsolete downstream relations)
    NOTE: Associated flood areas (incl. geospatial data in PostGIS) are not deleted
          as they may be used by other flood warnings (and are rather stable entities)

    Arguments:
        warnings_to_delete (list): List of flood warning URIs to be deleted

    Returns:
        Number of updated flood warnings as int
        List of IRIs for which to drop timestamps markup
    """
    
    # Initialise PostGIS client
    postgis_client = PostGISClient()

    # Create PySparqlClient instance if not provided
    if not kgclient:
        kgclient = PySparqlClient(query_endpoint=query_endpoint,
                                  update_endpoint=query_endpoint)

    # Initialise list of iris for which to drop timestamps markup
    drop_times = []

    for warning in warnings_to_delete:
        # Retrieve associated flood area URI
        query = get_associated_flood_area(warning)
        res = kgclient.performQuery(query)
        # Unwrap results
        try:
            area = [r['area_iri'] for r in res][0]
        except Exception as ex:
            logger.warning('No associated flood area IRI could be retrieved for warning: {}: {}'.format(warning, str(ex)))
            area = None
        
        if area:
            # Ensure flood area is set inactive (to be suppressed from visualising)
            num_rows = postgis_client.set_flood_area_activity(activity=False, area_uri=area)
            if num_rows != 1:
                logger.error(f'Expected to change "active" field for 1 flood area, but updated {num_rows}.')
                raise RuntimeError(f'Expected to change "active" field for 1 flood area, but updated {num_rows}.')

        # Append warning and derivation iris to list of iris for which to delete timestamps
        query = get_instances_with_timestamps_to_delete(warning)
        res = kgclient.performQuery(query)
        # Unwrap results
        drop_times.extend([iri for r in res for iri in list(r.values())])

        # Create SPARQL delete query for other triples
        query = delete_flood_warning(warning)
        # Perform update
        kgclient.performUpdate(query)

   
    return len(warnings_to_delete), drop_times


def get_instantiated_flood_warnings(query_endpoint=QUERY_ENDPOINT,
                                    kgclient=None) -> dict:
    """
    Retrieve all instantiated flood warnings with latest update timestamp

    Arguments:
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints

    Returns:
        warnings (dict): Dictionary with flood warning/alert IRIs as keys and
                         latest update timestamp as values
    """

    # Create PySparqlClient instance if not provided
    if not kgclient:
        kgclient = PySparqlClient(query_endpoint=query_endpoint,
                                  update_endpoint=query_endpoint)
    
    # Retrieve instantiated flood warnings
    query = get_all_flood_warnings()
    res = kgclient.performQuery(query)

    # Unwrap results
    warning = [r.pop('warning_iri') for r in res]
    last_altered = [list(r.values()) for r in res]
    for i in range(len(last_altered)):
        last_altered[i] = [dt.strptime(t, BLAZEGRAPH_TIME_FORMAT) for t in last_altered[i]]
        last_altered[i] = max(last_altered[i])
    warnings = dict(zip(warning, last_altered))

    return warnings


def get_instantiated_flood_areas(query_endpoint=QUERY_ENDPOINT,
                                 kgclient=None) -> dict:
    """
    Retrieve all instantiated flood areas with associated 'hasLocation' Location IRIs

    Arguments:
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints

    Returns:
        areas (dict): Dictionary with flood area IRIs as keys and associated 
                      Location IRIs as values
    """

    # Create PySparqlClient instance if not provided
    if not kgclient:
        kgclient = PySparqlClient(query_endpoint=query_endpoint,
                                  update_endpoint=query_endpoint)
    
    # Retrieve instantiated flood warnings
    query = get_all_flood_areas()
    res = kgclient.performQuery(query)

    # Unwrap results
    areas = {r['area_iri']: r['location_iri'] for r in res}

    return areas
