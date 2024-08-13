import os
import requests
import psycopg2
import logging

logging.basicConfig(level=logging.INFO)

from agent.layergenerator.env_for_layer import (
    GEOSERVER_URL,
    GEOSERVER_USER,
    GEOSERVER_PASSWORD,
    POSTGIS_HOST,
    POSTGIS_PORT,
    POSTGIS_DB,
    POSTGIS_USER,
    POSTGIS_PASSWORD,
    WORKSPACE
)

def execute_sql(connection, sql):
    with connection.cursor() as cursor:
        cursor.execute(sql)
    connection.commit()

def create_functions():
    connection = psycopg2.connect(
        dbname=POSTGIS_DB, user=POSTGIS_USER, password=POSTGIS_PASSWORD, host=POSTGIS_HOST, port=POSTGIS_PORT
    )

    get_column_name_function = """
    CREATE OR REPLACE FUNCTION getColumnName(iri VARCHAR)
    RETURNS VARCHAR AS $$
    DECLARE
        column_name VARCHAR;
    BEGIN
        SELECT "columnName" INTO column_name FROM "dbTable" WHERE "dataIRI" = iri;
        RETURN column_name;
    END;
    $$ LANGUAGE plpgsql;
    """

    get_table_name_function = """
    CREATE OR REPLACE FUNCTION getTableName(iri VARCHAR)
    RETURNS VARCHAR AS $$
    DECLARE
        table_name VARCHAR;
    BEGIN
        SELECT "tableName" INTO table_name FROM "dbTable" WHERE "dataIRI" = iri;
        RETURN table_name;
    END;
    $$ LANGUAGE plpgsql;
    """

    # Read the SQL query from the virtual_table.sql file
    with open('virtual_table.sql', 'r') as file:
        get_location_table_function = file.read()

    try:
        execute_sql(connection, get_column_name_function)
        execute_sql(connection, get_table_name_function)
        execute_sql(connection, get_location_table_function)
        logging.info("Created functions in PostGIS.")
    except Exception as e:
        logging.error("Error creating functions in PostGIS: %s", e)
        raise
    finally:
        connection.close()

def create_geoserver_layer(table_name):
    datastore_payload = f"""
    <dataStore>
      <name>{table_name}</name>
      <connectionParameters>
        <host>{POSTGIS_HOST}</host>
        <port>{POSTGIS_PORT}</port>
        <database>{POSTGIS_DB}</database>
        <user>{POSTGIS_USER}</user>
        <passwd>{POSTGIS_PASSWORD}</passwd>
        <dbtype>postgis</dbtype>
      </connectionParameters>
    </dataStore>
    """
    
    datastore_url = f"{GEOSERVER_URL}/rest/workspaces/{WORKSPACE}/datastores"
    datastore_response = requests.post(datastore_url, auth=(GEOSERVER_USER, GEOSERVER_PASSWORD), headers={"Content-type": "text/xml"}, data=datastore_payload)

    if not datastore_response.ok:
        logging.error("Failed to create datastore: %s", datastore_response.text)
        return datastore_response

    layer_payload = f"""
    <featureType>
      <name>{table_name}</name>
      <nativeName>{table_name}</nativeName>
      <title>{table_name}</title>
      <srs>EPSG:4326</srs>
    </featureType>
    """

    layer_url = f"{GEOSERVER_URL}/rest/workspaces/{WORKSPACE}/datastores/{table_name}/featuretypes"
    layer_response = requests.post(layer_url, auth=(GEOSERVER_USER, GEOSERVER_PASSWORD), headers={"Content-type": "text/xml"}, data=layer_payload)
    
    if not layer_response.ok:
        logging.error("Failed to create layer: %s", layer_response.text)
    
    return layer_response