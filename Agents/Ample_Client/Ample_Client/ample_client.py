import os
from pathlib import Path
import asyncio
from asyncua import Client
import csv
from datetime import datetime
import datetime
import sql_client
import utils

filePath = utils.get_env_variable("OPCUA_CONF")
url =utils.read_property(filePath, "opcua_server_url")

# Retrieve tag names and data types in the following structure: {tag_01_name:tag_01_data_type, tag_02_name:tag_02_data_type}
def read_tags_from_csv_file(client:Client, filename):
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

# Iterate throuh dict to retrieve value from OPC-UA server
# Insert retrieved values, data type, timestamps at point of retrieval into timeseries_dict
async def read_tag_values(client:Client, tags:dict):
    timeseries_dict={}
    for key, tag_and_data_type_dict in tags.items():
        values = {}
        # Read values from multiple nodes
        timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        data_values = await client.read_values(tag_and_data_type_dict["nodes"])
        for tag, value, data_type in zip(tag_and_data_type_dict["tags"], data_values, tag_and_data_type_dict["datatype"]):
            values.update({tag:{"timestamp":timestamp, "value":value, "data_type":data_type}})
        timeseries_dict.update({key:values})
    return timeseries_dict

async def main():
    client = Client(url=url)
    client.set_user(utils.read_property(filePath, "user"))
    client.set_password(utils.read_property(filePath, "password"))
    cert_base = Path(__file__).parent
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
            await asyncio.sleep(float(utils.read_property(filePath, "interval")))
    except Exception as e:
        print(f"Stopping due to an exception: {e}")
    finally:
        await client.disconnect()
        print("Disconnected safely.")
    

if __name__ == "__main__":
    asyncio.run(main())