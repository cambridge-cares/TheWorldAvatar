import os
from pathlib import Path
import asyncio
from asyncua import Client
import csv
from datetime import datetime, timezone
import datetime
from OPCUAAgent import sql_client
from OPCUAAgent import agent_utils

def read_tags_from_csv_file(client:Client, filename):
    """
    Read tags, datatype from csv file. \n
    Read sub identifier or prefix from csv file and combine it with tags to form the tags node id. \n
    Use OPCUA client to retrieve node instances for each tag node id.
    """
    tags = []
    datatype = []
    tags_nodeid=[]
    nodes=[]
    with open(filename, 'r') as file:
        reader = csv.reader(file)
        next(reader)
        for row in reader:
            tag = row[0].strip('"')
            data_type = row[1].strip('"')
            sub_identifier = row[2].strip('"')
            tags.append(tag)
            datatype.append(data_type)
            tags_nodeid.append(f"{sub_identifier}.{tag}")
            nodes.append(client.get_node(f"{sub_identifier}.{tag}"))
    return tags,datatype,tags_nodeid,nodes


def read_tags_from_csv_files(client:Client, folder_path):
    """
    Read tags, datatype from all csv files. \n
    Read sub identifier or prefix from all csv files and combine it with tags to form the tags node id. \n
    Use OPCUA client to retrieve node instances for each tag node id. \n
    Return all information as a dictionary. 
    """
    # Check if the folder path exists
    if not os.path.exists(folder_path):
        print(f"The folder '{folder_path}' does not exist.")
        return
    tags_dict={}
    # Iterate through all files in the folder
    for filename in os.listdir(folder_path):
        # Check if the file is a CSV file
        if filename.endswith('.csv'):
            file_path = os.path.join(folder_path, filename)
            print(f"Reading '{filename}':")
            try:
                tags, datatype,tags_nodeid, nodes = read_tags_from_csv_file(client=client, filename=file_path)
                tags_dict.update({filename.replace(".csv", ""):{"tags":tags, "datatype":datatype, "tags_nodeid":tags_nodeid, "nodes":nodes}})
            except Exception as e:
                print("Unable to retrieve tags from csv files.")
    # Return all retrieved tags as a dictionary
    return tags_dict

async def read_tag_values(client:Client, tags:dict):
    """
    Iterate through dictionary and read tag values from OPCUA server. \n
    Return dictionary containing retrieved values, data type, timestamps at point of retrieval.
    """
    timeseries_dict={}
    for key, tag_and_data_type_dict in tags.items():
        values = {}
        # Read values from multiple nodes
        timestamp = datetime.datetime.now(timezone.utc).isoformat(timespec='seconds')
        data_values = await client.read_values(tag_and_data_type_dict["nodes"])
        for tag, value, data_type in zip(tag_and_data_type_dict["tags"], data_values, tag_and_data_type_dict["datatype"]):
            values.update({tag:{"timestamp":timestamp, "value":value, "data_type":data_type}})
        timeseries_dict.update({key:values})
    return timeseries_dict

async def main():
    """
    Main method that establishes a connection to the OPCUA server, \n
    retrieve the timeseries data of all the tags indicated in the csv files, \n
    insert the timeseries data into a PostgreSQL database.
    """
    filePath = agent_utils.get_env_variable("OPCUA_CONF")
    url =agent_utils.read_property(filePath, "opcua_server_url")
    client = Client(url=url)
    client.set_user(agent_utils.read_property(filePath, "user"))
    client.set_password(agent_utils.read_property(filePath, "password"))
    print("connecting to the following endpoint: " + url)
    try:
        await client.connect()
        while True:
            base = Path(__file__).parent
            tags = read_tags_from_csv_files(client=client, folder_path=base / "filtered_tags")
            timeseries_dict = await read_tag_values(client, tags)
            sql_client.create_database_if_not_exist()
            connection = sql_client.connect_to_database()
            sql_client.create_if_not_exist_and_insert(connection=connection, dict=timeseries_dict)
            await asyncio.sleep(float(agent_utils.read_property(filePath, "interval")))
    except Exception as e:
        print(f"Stopping due to an exception: {e}")
    finally:
        await client.disconnect()
        print("Disconnected safely.")
    

if __name__ == "__main__":
    asyncio.run(main())