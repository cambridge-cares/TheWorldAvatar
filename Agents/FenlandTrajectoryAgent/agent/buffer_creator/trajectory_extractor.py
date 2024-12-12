import psycopg2
import logging
from typing import Optional, Any
import pandas as pd

def connect_to_database(
    host: str, port: int, user: str, password: str, database: str
) -> psycopg2.extensions.connection:
    """
    Establish a connection to the PostgreSQL database.
    Raises an exception if the connection fails.
    """
    try:
        logging.info("Starting database connection...")
        connection = psycopg2.connect(
            host=host,
            port=port,
            user=user,
            password=password,
            database=database,
            connect_timeout=10
        )
        logging.info("Connected to the database.")
        return connection
    except psycopg2.Error as e:
        logging.error(f"Error connecting to the database: {e}")
        raise e


def execute_query(connection: psycopg2.extensions.connection, query: str, params: Optional[tuple] = None) -> Any:
    """
    Execute a query on the database and fetch results.
    Returns the results of the query.
    """
    try:
        with connection.cursor() as cursor:
            logging.info(f"Executing query: {query}")
            cursor.execute(query, params)
            results = cursor.fetchall()
            if results:
                logging.info(f"Query returned {len(results)} rows.")
            else:
                logging.info("Query returned no results.")
            return results
    except psycopg2.Error as e:
        logging.error(f"Error executing query: {e}")
        raise e


def get_table_name_for_timeseries(connection, timeseriesIRI: str) -> str:
    """
    Given a timeseriesIRI, query dbTable to find the unique tableName.
    """
    query = """
    SELECT DISTINCT "tableName" 
    FROM "dbTable"
    WHERE "timeseriesIRI" = %s;
    """
    results = execute_query(connection, query, (timeseriesIRI,))
    if not results or len(results) == 0:
        raise ValueError(f"No tableName found for timeseriesIRI: {timeseriesIRI}")
    table_name = results[0][0]
    return table_name


def get_timeseries_data(connection, table_name: str):
    """
    Given the table name, query the time and column1 to column7
    and return a DataFrame.
    """
    query = f"""
    SELECT "time", "column1", "column2", "column3", "column4", "column5", "column6", "column7"
    FROM "{table_name}";
    """
    results = execute_query(connection, query)
    df = pd.DataFrame(results, columns=["time","column1","column2","column3","column4","column5","column6","column7"])
    return df


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)

    # Connection details
    host = "localhost"
    port = 5432
    user = "postgres"
    password = "1111"
    database = "postgres"

    timeseriesIRI = "https://www.theworldavatar.com/kg/ontotimeseries/Timeseries_a57a5978-0d2f-41b3-ab35-35944166f322"

    try:
        with connect_to_database(host, port, user, password, database) as conn:
            table_name = get_table_name_for_timeseries(conn, timeseriesIRI)
            logging.info(f"Found table name: {table_name}")

            df = get_timeseries_data(conn, table_name)

            for index, row in df.iterrows():

                print(row.to_dict())
    except Exception as e:
        logging.error(f"An error occurred: {e}")
