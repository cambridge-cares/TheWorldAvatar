import os
import psycopg2
from psycopg2 import sql
from OPCUAAgent import agent_utils
import re

# Check whether a database exist and create it if not
def create_database_if_not_exist():
    try:
        filePath = agent_utils.get_env_variable("POSTGRES_CONF")
        connection = psycopg2.connect(
            dbname="postgres",
            user=agent_utils.read_property(filePath, "user"),
            password=agent_utils.read_property(filePath, "password"),
            host=agent_utils.read_property(filePath, "host"),
            port=agent_utils.read_property(filePath, "port")
        )
        connection.autocommit = True
        
        dbname=agent_utils.read_property(filePath, "dbname")
        cursor = connection.cursor()
        # Check if the database exists
        cursor.execute(
            sql.SQL("SELECT 1 FROM pg_database WHERE datname = %s"),
            [dbname]
        )
        exists = cursor.fetchone()
                
        if not exists:
            cursor.execute(
            sql.SQL("CREATE DATABASE {}").format(
                sql.Identifier(dbname)
                )
        )
    except (Exception, psycopg2.Error) as error:
        print("Error while connecting to PostgreSQL:", error)
        raise Exception("Error while connecting to PostgreSQL, do check the environment variable and properties file...")
    
# Function to establish a connection to the PostgreSQL database
def connect_to_database():
    try:
        filePath = agent_utils.get_env_variable("POSTGRES_CONF")     
        connection = psycopg2.connect(
            dbname=agent_utils.read_property(filePath, "dbname"),
            user=agent_utils.read_property(filePath, "user"),
            password=agent_utils.read_property(filePath, "password"),
            host=agent_utils.read_property(filePath, "host"),
            port=agent_utils.read_property(filePath, "port")
        )
        return connection
    except (Exception, psycopg2.Error) as error:
        print("Error while connecting to PostgreSQL:" + str(error))
        raise Exception("Error while connecting to PostgreSQL:" + str(error))
        

# Function to create schemas and tables
def create_if_not_exist_and_insert(connection, dict:dict):
        cursor = connection.cursor()
        schema_name = "opcua_pips"
        cursor.execute(sql.SQL("CREATE SCHEMA IF NOT EXISTS {}").format(sql.Identifier(str(schema_name))))
        # Iterate through the dictionary
        for table_name, timeseries_dict in dict.items():
            # Create tables in the schema with timestamp column
            cursor.execute(sql.SQL("CREATE TABLE IF NOT EXISTS {}.{} (timestamp TIMESTAMP WITH TIME ZONE)").format(
                sql.Identifier(str(schema_name)), sql.Identifier(str(table_name))))

            # Retrieve all columns
            cursor.execute(f"SELECT column_name FROM information_schema.columns WHERE table_name = %s;",
                           (table_name,))
            existing_columns = [row[0] for row in cursor.fetchall()]
            print(existing_columns)
            column_list = ['timestamp']
            value_list = [str(timeseries_dict[list(timeseries_dict.keys())[0]]['timestamp'])]
            for key, timeseries in timeseries_dict.items():
                key = key.replace(' ','_')
                key = remove_text_between_characters(key, '(', ')')
                key = remove_text_between_characters(key, '[', ']')
                if key.endswith('_'):
                    key = key[:-1]
                data_type = timeseries['data_type']
                if data_type == "Float":
                    data_type = "DOUBLE PRECISION"
                # Add column if not exist
                try:
                    if key.lower() not in existing_columns:
                        cursor.execute(sql.SQL("ALTER TABLE {}.{} ADD COLUMN {} {} ;").format(
                        sql.Identifier(str(schema_name)), sql.Identifier(str(table_name)), sql.SQL(key), sql.SQL(data_type)))
                    column_list.append(str(key))
                    value_list.append((timeseries['value']))
                except (Exception, psycopg2.Error) as error:
                    print("Error while creating table:", error)
            
            try:
                timestamp = str(timeseries_dict[list(timeseries_dict.keys())[0]]['timestamp'])
                # Check if the timestamp already exists in the table
                cursor.execute(
                    sql.SQL("SELECT 1 FROM {}.{} WHERE timestamp = %s").format(
                    sql.Identifier(str(schema_name)), sql.Identifier(str(table_name))), (timestamp,))
                existing_record = cursor.fetchone()
                    
                if not existing_record:  
                    query = f"INSERT INTO {schema_name}.{table_name} ({', '.join(column_list)}) VALUES ({', '.join(['%s'] * len(column_list))});"
                    # Execute the query with values
                    cursor.execute(query, value_list)
                    print("Data inserted successfully.")
            except (Exception, psycopg2.Error) as error:
                print("Error while inserting data:", error)
                    
        connection.commit()
        cursor.close()
    
def remove_text_between_characters(text, start_char, end_char):
    # Define the regular expression pattern to match text between the specified characters
    pattern = re.escape(start_char) + r'.*?' + re.escape(end_char)
    
    # Use re.sub() to replace the matched pattern with an empty string
    cleaned_text = re.sub(pattern, '', text)
    
    return cleaned_text