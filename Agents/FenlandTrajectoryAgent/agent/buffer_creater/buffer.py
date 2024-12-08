import psycopg2
import logging

def connect_to_database(host, port, user, password, database):
    try:
        logging.info("Starting database connection...")
        connection = psycopg2.connect(
            host=host,
            port=port,
            user=user,
            password=password,
            database=database
        )
        logging.info("Connected to the database.")
        return connection
    except psycopg2.Error as e:
        logging.error(f"Error connecting to the database: {e}")
        return None

def execute_query(connection, query):
    try:
        cursor = connection.cursor()
        logging.info("Executing query...")
        cursor.execute(query)
        results = cursor.fetchall()
        logging.info("Query executed. Fetching results...")
        return results
    except psycopg2.Error as e:
        logging.error(f"Error executing query: {e}")
        return None
    finally:
        if 'cursor' in locals():
            cursor.close()
            logging.info("Cursor closed.")

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)

    # Connection details
    host = "174.138.27.240"
    port = 5432
    user = "postgres"
    password = "1111"
    database = "postgres"

    # SQL Query
    query = 'SELECT "UTC TIME" FROM "gps_tra_2016";'

    # Connect to database
    conn = connect_to_database(host, port, user, password, database)

    if conn:
        results = execute_query(conn, query)
        if results:
            for row in results:
                print(row[0])
        conn.close()
        logging.info("Database connection closed.")
